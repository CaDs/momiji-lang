use std::collections::{HashMap, HashSet, VecDeque};

use crate::check::cache::CacheEntry;
use crate::parser::ast::{Block, Expr, Item, Program, Stmt};

/// Names exported and called by a single file.
pub struct FileInfo {
    pub exports: HashSet<String>,
    pub calls: HashSet<String>,
}

const BUILTINS: &[&str] = &["puts", "print"];

/// Extract exported function names and called function names from a parsed AST.
pub fn extract_file_info(program: &Program) -> FileInfo {
    let mut exports = HashSet::new();
    let mut calls = HashSet::new();

    for item in &program.items {
        let Item::Function(function) = item;
        exports.insert(function.name.clone());
        collect_calls_block(&function.body, &mut calls);
    }

    FileInfo { exports, calls }
}

/// Compute the external dependencies: calls that are neither local exports nor builtins.
pub fn external_deps(info: &FileInfo) -> Vec<String> {
    let builtin_set: HashSet<&str> = BUILTINS.iter().copied().collect();
    let mut deps: Vec<String> = info
        .calls
        .iter()
        .filter(|name| {
            !info.exports.contains(name.as_str()) && !builtin_set.contains(name.as_str())
        })
        .cloned()
        .collect();
    deps.sort();
    deps
}

/// File-level dependency graph for precise invalidation.
#[allow(dead_code)]
pub struct DepGraph {
    /// Maps function name -> file key that exports it.
    provider: HashMap<String, String>,
    /// Maps file key -> set of file keys that depend on it (reverse edges).
    pub depended_by: HashMap<String, HashSet<String>>,
    /// Files with deps that no provider supplies.
    pub has_unknown_deps: HashSet<String>,
}

impl DepGraph {
    /// Build the dependency graph from cache entries.
    pub fn build(entries: &HashMap<String, CacheEntry>) -> Self {
        let mut provider: HashMap<String, String> = HashMap::new();
        let mut depended_by: HashMap<String, HashSet<String>> = HashMap::new();
        let mut has_unknown_deps: HashSet<String> = HashSet::new();

        // First pass: map each exported function to its provider file.
        for (file_key, entry) in entries {
            for export in &entry.exports {
                provider.insert(export.clone(), file_key.clone());
            }
        }

        // Second pass: for each file's external deps, create reverse edges.
        for (file_key, entry) in entries {
            for dep in &entry.external_deps {
                if let Some(provider_key) = provider.get(dep) {
                    depended_by
                        .entry(provider_key.clone())
                        .or_default()
                        .insert(file_key.clone());
                } else {
                    has_unknown_deps.insert(file_key.clone());
                }
            }
        }

        DepGraph {
            provider,
            depended_by,
            has_unknown_deps,
        }
    }
}

/// Given the set of fingerprint-changed file keys and API-changed file keys,
/// compute the precise set of file keys that need rechecking.
pub fn compute_invalidation_set(
    graph: &DepGraph,
    fingerprint_changed: &HashSet<String>,
    api_changed: &HashSet<String>,
    deleted_exports: &HashSet<String>,
) -> HashSet<String> {
    let mut recheck = fingerprint_changed.clone();

    // BFS from each API-changed file to find transitive dependents.
    let mut queue: VecDeque<String> = VecDeque::new();
    let mut visited: HashSet<String> = HashSet::new();

    for key in api_changed {
        if visited.insert(key.clone()) {
            queue.push_back(key.clone());
        }
    }

    // Also seed BFS for files that provided deleted exports — their dependents need rechecking.
    for export in deleted_exports {
        if let Some(dependents) = graph.depended_by.get(export) {
            for dep_key in dependents {
                recheck.insert(dep_key.clone());
            }
        }
    }

    while let Some(key) = queue.pop_front() {
        if let Some(dependents) = graph.depended_by.get(&key) {
            for dep_key in dependents {
                recheck.insert(dep_key.clone());
                if visited.insert(dep_key.clone()) {
                    queue.push_back(dep_key.clone());
                }
            }
        }
    }

    // Files with unknown deps are conservatively rechecked if any API changed.
    if !api_changed.is_empty() || !deleted_exports.is_empty() {
        for key in &graph.has_unknown_deps {
            recheck.insert(key.clone());
        }
    }

    recheck
}

fn collect_calls_block(block: &Block, calls: &mut HashSet<String>) {
    for stmt in &block.stmts {
        collect_calls_stmt(stmt, calls);
    }
}

fn collect_calls_stmt(stmt: &Stmt, calls: &mut HashSet<String>) {
    match stmt {
        Stmt::Expr(expr) => collect_calls_expr(expr, calls),
        Stmt::Let { value, .. } => collect_calls_expr(value, calls),
        Stmt::Return { value, .. } => {
            if let Some(expr) = value {
                collect_calls_expr(expr, calls);
            }
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            collect_calls_expr(condition, calls);
            collect_calls_block(then_block, calls);
            if let Some(else_blk) = else_block {
                collect_calls_block(else_blk, calls);
            }
        }
        Stmt::While {
            condition, body, ..
        } => {
            collect_calls_expr(condition, calls);
            collect_calls_block(body, calls);
        }
        Stmt::For {
            iterable, body, ..
        } => {
            collect_calls_expr(iterable, calls);
            collect_calls_block(body, calls);
        }
    }
}

fn collect_calls_expr(expr: &Expr, calls: &mut HashSet<String>) {
    match expr {
        Expr::Call { callee, args, .. } => {
            if let Expr::Identifier { name, .. } = callee.as_ref() {
                calls.insert(name.clone());
            } else {
                collect_calls_expr(callee, calls);
            }
            for arg in args {
                collect_calls_expr(arg, calls);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_calls_expr(left, calls);
            collect_calls_expr(right, calls);
        }
        Expr::Unary { expr: inner, .. } => {
            collect_calls_expr(inner, calls);
        }
        Expr::Field { object, .. } => {
            collect_calls_expr(object, calls);
        }
        Expr::Index { object, index, .. } => {
            collect_calls_expr(object, calls);
            collect_calls_expr(index, calls);
        }
        Expr::Array { elements, .. } => {
            for el in elements {
                collect_calls_expr(el, calls);
            }
        }
        Expr::Grouped { expr: inner, .. } => {
            collect_calls_expr(inner, calls);
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::cache::{CacheEntry, FileFingerprint};
    use crate::lexer::Scanner;
    use crate::parser::Parser as MojiParser;

    fn parse(source: &str) -> Program {
        let tokens = Scanner::new(source).scan_all().unwrap();
        MojiParser::new(tokens).parse().unwrap()
    }

    fn dummy_entry(
        exports: &[&str],
        external_deps: &[&str],
        api_hash: u64,
    ) -> CacheEntry {
        CacheEntry {
            fingerprint: FileFingerprint {
                mtime_nanos: 0,
                size: 0,
            },
            api_hash,
            exports: exports.iter().map(|s| s.to_string()).collect(),
            external_deps: external_deps.iter().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn extract_exports_and_calls() {
        let ast = parse(
            "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n\
             def main\n  puts(add(1, 2))\nend\n",
        );
        let info = extract_file_info(&ast);
        assert_eq!(
            info.exports,
            HashSet::from(["add".to_string(), "main".to_string()])
        );
        assert_eq!(
            info.calls,
            HashSet::from(["puts".to_string(), "add".to_string()])
        );
    }

    #[test]
    fn external_deps_excludes_locals_and_builtins() {
        let ast = parse(
            "def helper -> Int\n  return 1\nend\n\
             def main\n  puts(helper())\n  foo()\nend\n",
        );
        let info = extract_file_info(&ast);
        let deps = external_deps(&info);
        assert_eq!(deps, vec!["foo".to_string()]);
    }

    #[test]
    fn calls_in_nested_control_flow() {
        let ast = parse(
            "def main\n\
               if true\n    a()\n  else\n    b()\n  end\n\
               while true\n    c()\n  end\n\
               for i in d()\n    e()\n  end\n\
             end\n",
        );
        let info = extract_file_info(&ast);
        assert!(info.calls.contains("a"));
        assert!(info.calls.contains("b"));
        assert!(info.calls.contains("c"));
        assert!(info.calls.contains("d"));
        assert!(info.calls.contains("e"));
    }

    #[test]
    fn dep_graph_build_creates_correct_edges() {
        // A exports "add", B calls "add", C is independent.
        let mut entries = HashMap::new();
        entries.insert("a.mj".to_string(), dummy_entry(&["add"], &[], 1));
        entries.insert("b.mj".to_string(), dummy_entry(&["use_add"], &["add"], 2));
        entries.insert("c.mj".to_string(), dummy_entry(&["mul"], &[], 3));

        let graph = DepGraph::build(&entries);

        assert_eq!(graph.provider.get("add"), Some(&"a.mj".to_string()));
        assert_eq!(graph.provider.get("use_add"), Some(&"b.mj".to_string()));
        assert!(graph.depended_by.get("a.mj").unwrap().contains("b.mj"));
        assert!(!graph.depended_by.contains_key("c.mj"));
        assert!(graph.has_unknown_deps.is_empty());
    }

    #[test]
    fn dep_graph_unknown_deps() {
        let mut entries = HashMap::new();
        entries.insert("a.mj".to_string(), dummy_entry(&["foo"], &["unknown_fn"], 1));

        let graph = DepGraph::build(&entries);
        assert!(graph.has_unknown_deps.contains("a.mj"));
    }

    #[test]
    fn invalidation_set_api_change_propagates() {
        // A exports "add", B calls "add", C is independent.
        let mut entries = HashMap::new();
        entries.insert("a.mj".to_string(), dummy_entry(&["add"], &[], 1));
        entries.insert("b.mj".to_string(), dummy_entry(&["use_add"], &["add"], 2));
        entries.insert("c.mj".to_string(), dummy_entry(&["mul"], &[], 3));

        let graph = DepGraph::build(&entries);

        let fingerprint_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);
        let api_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);

        let recheck =
            compute_invalidation_set(&graph, &fingerprint_changed, &api_changed, &HashSet::new());

        assert!(recheck.contains("a.mj"));
        assert!(recheck.contains("b.mj"));
        assert!(!recheck.contains("c.mj"));
    }

    #[test]
    fn invalidation_set_body_only_no_downstream() {
        let mut entries = HashMap::new();
        entries.insert("a.mj".to_string(), dummy_entry(&["add"], &[], 1));
        entries.insert("b.mj".to_string(), dummy_entry(&["use_add"], &["add"], 2));

        let graph = DepGraph::build(&entries);

        // A changed fingerprint but NOT API.
        let fingerprint_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);
        let api_changed: HashSet<String> = HashSet::new();

        let recheck =
            compute_invalidation_set(&graph, &fingerprint_changed, &api_changed, &HashSet::new());

        assert!(recheck.contains("a.mj"));
        assert!(!recheck.contains("b.mj"));
    }

    #[test]
    fn invalidation_set_transitive_chain() {
        // A -> B -> C chain (A exports "f", B calls "f" and exports "g", C calls "g").
        let mut entries = HashMap::new();
        entries.insert("a.mj".to_string(), dummy_entry(&["f"], &[], 1));
        entries.insert("b.mj".to_string(), dummy_entry(&["g"], &["f"], 2));
        entries.insert("c.mj".to_string(), dummy_entry(&["h"], &["g"], 3));

        let graph = DepGraph::build(&entries);

        let fingerprint_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);
        let api_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);

        let recheck =
            compute_invalidation_set(&graph, &fingerprint_changed, &api_changed, &HashSet::new());

        assert!(recheck.contains("a.mj"));
        assert!(recheck.contains("b.mj"));
        // C depends on B (not A directly), and B's API didn't change in this pass,
        // so C is NOT transitively rechecked — this is correct for single-level propagation.
        // However our BFS traverses depended_by edges from A -> B, but B -> C edge
        // means B is in the recheck set, and the BFS also visits B's dependents.
        // Actually the BFS goes: A is api_changed, look at A's dependents (B),
        // add B to recheck, then look at B's dependents (C), add C to recheck.
        // So transitive propagation happens.
        assert!(recheck.contains("c.mj"));
    }

    #[test]
    fn invalidation_set_circular_deps() {
        // A calls B's func, B calls A's func.
        let mut entries = HashMap::new();
        entries.insert("a.mj".to_string(), dummy_entry(&["fa"], &["fb"], 1));
        entries.insert("b.mj".to_string(), dummy_entry(&["fb"], &["fa"], 2));

        let graph = DepGraph::build(&entries);

        let fingerprint_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);
        let api_changed: HashSet<String> =
            HashSet::from(["a.mj".to_string()]);

        let recheck =
            compute_invalidation_set(&graph, &fingerprint_changed, &api_changed, &HashSet::new());

        assert!(recheck.contains("a.mj"));
        assert!(recheck.contains("b.mj"));
    }
}
