use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use miette::{IntoDiagnostic, WrapErr};

use crate::parser::ast::TypeKind;

const CACHE_FORMAT_VERSION: u32 = 1;

fn compiler_version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

pub struct CacheLoadResult {
    pub cache: HashMap<String, CacheEntry>,
    pub version_invalidated: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileFingerprint {
    pub mtime_nanos: u128,
    pub size: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CacheEntry {
    pub fingerprint: FileFingerprint,
    pub api_hash: u64,
    pub exports: Vec<String>,
    pub external_deps: Vec<String>,
}

/// Result of parsing a file once and extracting both API hash and dependency info.
pub struct FileMetadata {
    pub api_hash: u64,
    pub exports: Vec<String>,
    pub external_deps: Vec<String>,
}

pub fn check_cache_path() -> PathBuf {
    if let Ok(dir) = std::env::var("MOMIJI_CACHE_DIR") {
        return PathBuf::from(dir).join("check-cache.tsv");
    }

    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(".momiji")
        .join("check-cache.tsv")
}

pub fn cache_key(file: &Path) -> String {
    let abs = if file.is_absolute() {
        file.to_path_buf()
    } else if let Ok(cwd) = std::env::current_dir() {
        cwd.join(file)
    } else {
        file.to_path_buf()
    };

    abs.to_string_lossy().to_string()
}

pub fn file_fingerprint(file: &Path) -> miette::Result<FileFingerprint> {
    let metadata = fs::metadata(file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat source file: {}", file.display()))?;

    let modified = metadata.modified().into_diagnostic().wrap_err_with(|| {
        format!(
            "Failed to read modification timestamp for source file: {}",
            file.display()
        )
    })?;
    let modified = modified
        .duration_since(UNIX_EPOCH)
        .into_diagnostic()
        .wrap_err_with(|| {
            format!(
                "Source file has an invalid modification timestamp: {}",
                file.display()
            )
        })?;

    Ok(FileFingerprint {
        mtime_nanos: modified.as_nanos(),
        size: metadata.len(),
    })
}

/// Parse a file once and extract both its API hash and dependency info.
pub fn compute_file_metadata(file: &Path) -> miette::Result<FileMetadata> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    use miette::{NamedSource, Report};

    use crate::check::deps::{extract_file_info, external_deps};
    use crate::lexer::Scanner;
    use crate::parser::ast::Item;
    use crate::parser::Parser as MojiParser;

    let source = fs::read_to_string(file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", file.display()))?;

    let file_name = file
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_else(|| file.display().to_string());

    let tokens = Scanner::new(&source).scan_all().map_err(|e| {
        Report::new(e).with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;
    let ast = MojiParser::new(tokens).parse().map_err(|e| {
        Report::new(e).with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;

    // API hash from function and struct signatures
    let mut hasher = DefaultHasher::new();
    for item in &ast.items {
        match item {
            Item::Function(function) => {
                "fn".hash(&mut hasher);
                function.name.hash(&mut hasher);
                function.params.len().hash(&mut hasher);
                for param in &function.params {
                    param.name.hash(&mut hasher);
                    type_kind_fingerprint(&param.ty.kind).hash(&mut hasher);
                }
                match &function.return_type {
                    Some(annotation) => type_kind_fingerprint(&annotation.kind).hash(&mut hasher),
                    None => "Unit".hash(&mut hasher),
                }
            }
            Item::Struct(struct_def) => {
                "struct".hash(&mut hasher);
                struct_def.name.hash(&mut hasher);
                struct_def.fields.len().hash(&mut hasher);
                for field in &struct_def.fields {
                    field.name.hash(&mut hasher);
                    type_kind_fingerprint(&field.ty.kind).hash(&mut hasher);
                }
            }
        }
    }
    let api_hash = hasher.finish();

    // Dependency info
    let info = extract_file_info(&ast);
    let mut exports: Vec<String> = info.exports.iter().cloned().collect();
    exports.sort();
    let ext_deps = external_deps(&info);

    Ok(FileMetadata {
        api_hash,
        exports,
        external_deps: ext_deps,
    })
}

pub fn type_kind_fingerprint(kind: &TypeKind) -> String {
    match kind {
        TypeKind::Named(name) => format!("N:{name}"),
        TypeKind::Nullable(inner) => format!("Q:{}", type_kind_fingerprint(inner)),
        TypeKind::Array(inner) => format!("A:{}", type_kind_fingerprint(inner)),
    }
}

pub fn load_check_cache(path: &Path) -> miette::Result<CacheLoadResult> {
    if !path.exists() {
        return Ok(CacheLoadResult {
            cache: HashMap::new(),
            version_invalidated: false,
        });
    }

    let contents = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read check cache at {}", path.display()))?;

    if contents.is_empty() {
        return Ok(CacheLoadResult {
            cache: HashMap::new(),
            version_invalidated: false,
        });
    }

    let mut lines = contents.lines();
    let first_line = lines.next().unwrap_or("");

    // Check for header line
    if let Some(rest) = first_line.strip_prefix("# momiji-cache ") {
        let expected = format!("v{} compiler={}", CACHE_FORMAT_VERSION, compiler_version());
        if rest != expected {
            return Ok(CacheLoadResult {
                cache: HashMap::new(),
                version_invalidated: true,
            });
        }
        // Header matches, parse remaining lines as data
    } else if first_line.starts_with('#') {
        // Some other comment header we don't recognize
        return Ok(CacheLoadResult {
            cache: HashMap::new(),
            version_invalidated: true,
        });
    } else {
        // No header (old format) — first line is data, treat as version mismatch
        return Ok(CacheLoadResult {
            cache: HashMap::new(),
            version_invalidated: true,
        });
    }

    let mut cache = HashMap::new();
    for line in lines {
        if line.starts_with('#') {
            continue;
        }
        parse_cache_data_line(line, &mut cache);
    }

    Ok(CacheLoadResult {
        cache,
        version_invalidated: false,
    })
}

fn parse_cache_data_line(line: &str, cache: &mut HashMap<String, CacheEntry>) {
    let columns: Vec<&str> = line.split('\t').collect();
    if columns.len() < 3 {
        return;
    }
    let key = columns[0];
    let Ok(mtime_nanos) = columns[1].parse::<u128>() else {
        return;
    };
    let Ok(size) = columns[2].parse::<u64>() else {
        return;
    };
    let api_hash = columns
        .get(3)
        .and_then(|raw| raw.parse::<u64>().ok())
        .unwrap_or(0);

    let exports = columns
        .get(4)
        .filter(|s| !s.is_empty())
        .map(|s| s.split(',').map(String::from).collect())
        .unwrap_or_default();
    let external_deps = columns
        .get(5)
        .filter(|s| !s.is_empty())
        .map(|s| s.split(',').map(String::from).collect())
        .unwrap_or_default();

    cache.insert(
        key.to_string(),
        CacheEntry {
            fingerprint: FileFingerprint { mtime_nanos, size },
            api_hash,
            exports,
            external_deps,
        },
    );
}

pub fn save_check_cache(path: &Path, cache: &HashMap<String, CacheEntry>) -> miette::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .into_diagnostic()
            .wrap_err_with(|| {
                format!(
                    "Failed to create check cache directory {}",
                    parent.display()
                )
            })?;
    }

    let mut entries: Vec<_> = cache.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));

    let mut out = String::new();
    out.push_str(&format!(
        "# momiji-cache v{} compiler={}\n",
        CACHE_FORMAT_VERSION,
        compiler_version()
    ));
    for (key, value) in entries {
        out.push_str(key);
        out.push('\t');
        out.push_str(&value.fingerprint.mtime_nanos.to_string());
        out.push('\t');
        out.push_str(&value.fingerprint.size.to_string());
        out.push('\t');
        out.push_str(&value.api_hash.to_string());
        out.push('\t');
        out.push_str(&value.exports.join(","));
        out.push('\t');
        out.push_str(&value.external_deps.join(","));
        out.push('\n');
    }

    fs::write(path, out)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write check cache at {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn temp_cache_file(contents: &str) -> NamedTempFile {
        let mut f = NamedTempFile::new().unwrap();
        f.write_all(contents.as_bytes()).unwrap();
        f
    }

    fn valid_header() -> String {
        format!(
            "# momiji-cache v{} compiler={}\n",
            CACHE_FORMAT_VERSION,
            compiler_version()
        )
    }

    #[test]
    fn load_cache_with_valid_header() {
        let contents = format!(
            "{}/path/a.mj\t1000\t200\t999\tadd,sub\tmul\n",
            valid_header()
        );
        let f = temp_cache_file(&contents);
        let result = load_check_cache(f.path()).unwrap();
        assert!(!result.version_invalidated);
        let entry = result.cache.get("/path/a.mj").unwrap();
        assert_eq!(entry.fingerprint.mtime_nanos, 1000);
        assert_eq!(entry.fingerprint.size, 200);
        assert_eq!(entry.api_hash, 999);
        assert_eq!(entry.exports, vec!["add", "sub"]);
        assert_eq!(entry.external_deps, vec!["mul"]);
    }

    #[test]
    fn load_cache_with_mismatched_format_version() {
        let f = temp_cache_file("# momiji-cache v999 compiler=0.1.0\n/path/a.mj\t1\t2\t3\n");
        let result = load_check_cache(f.path()).unwrap();
        assert!(result.version_invalidated);
        assert!(result.cache.is_empty());
    }

    #[test]
    fn load_cache_with_mismatched_compiler_version() {
        let contents = format!(
            "# momiji-cache v{} compiler=99.99.99\n/path/a.mj\t1\t2\t3\n",
            CACHE_FORMAT_VERSION
        );
        let f = temp_cache_file(&contents);
        let result = load_check_cache(f.path()).unwrap();
        assert!(result.version_invalidated);
        assert!(result.cache.is_empty());
    }

    #[test]
    fn load_cache_without_header_old_format() {
        let f = temp_cache_file("/path/a.mj\t1000\t200\t999\n");
        let result = load_check_cache(f.path()).unwrap();
        assert!(result.version_invalidated);
        assert!(result.cache.is_empty());
    }

    #[test]
    fn round_trip_preserves_header() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("cache.tsv");

        let mut cache = HashMap::new();
        cache.insert(
            "/path/a.mj".to_string(),
            CacheEntry {
                fingerprint: FileFingerprint {
                    mtime_nanos: 1000,
                    size: 200,
                },
                api_hash: 42,
                exports: vec!["add".to_string(), "sub".to_string()],
                external_deps: vec!["mul".to_string()],
            },
        );

        save_check_cache(&path, &cache).unwrap();

        // Verify header is present in the file
        let raw = std::fs::read_to_string(&path).unwrap();
        assert!(raw.starts_with("# momiji-cache v"));

        let result = load_check_cache(&path).unwrap();
        assert!(!result.version_invalidated);
        assert_eq!(cache, result.cache);
    }

    #[test]
    fn load_empty_file() {
        let f = temp_cache_file("");
        let result = load_check_cache(f.path()).unwrap();
        assert!(!result.version_invalidated);
        assert!(result.cache.is_empty());
    }
}
