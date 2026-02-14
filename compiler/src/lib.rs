pub mod check;
pub mod codegen;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod sem;
pub mod types;

pub use codegen::Codegen;
pub use runtime::Interpreter;
pub use sem::SemLowerer;

use std::fs;
use std::path::Path;
use std::time::{Duration, Instant};

use miette::{IntoDiagnostic, NamedSource, Report, WrapErr};

use crate::sem::ir::SemProgram;

pub struct FrontendArtifacts {
    pub sem: SemProgram,
    pub source: String,
    pub file_name: String,
    pub timing: FrontendTiming,
}

#[derive(Clone, Copy, Debug)]
pub struct FrontendTiming {
    pub read: Duration,
    pub lex: Duration,
    pub parse: Duration,
    pub sem: Duration,
}

impl FrontendTiming {
    pub fn total(self) -> Duration {
        self.read + self.lex + self.parse + self.sem
    }
}

pub fn frontend(file: &Path) -> miette::Result<FrontendArtifacts> {
    let read_start = Instant::now();
    let source = fs::read_to_string(file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", file.display()))?;
    let read = read_start.elapsed();

    let file_name = file
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_else(|| file.display().to_string());

    // Lexing
    let lex_start = Instant::now();
    let tokens = lexer::Scanner::new(&source).scan_all().map_err(|e| {
        Report::new(e).with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;
    let lex = lex_start.elapsed();

    // Parsing
    let parse_start = Instant::now();
    let ast = parser::Parser::new(tokens).parse().map_err(|e| {
        Report::new(e).with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;
    let parse = parse_start.elapsed();

    // Semantic lowering / validation
    let sem_start = Instant::now();
    let sem = sem::SemLowerer::new().lower(&ast).map_err(|errors| {
        Report::new(errors.into_iter().next().unwrap())
            .with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;
    let sem_time = sem_start.elapsed();

    Ok(FrontendArtifacts {
        sem,
        source,
        file_name,
        timing: FrontendTiming {
            read,
            lex,
            parse,
            sem: sem_time,
        },
    })
}
