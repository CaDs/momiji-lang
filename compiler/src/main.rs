use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use clap::{Parser, Subcommand, ValueEnum};
use miette::{IntoDiagnostic, NamedSource, Report, WrapErr};

use momiji::codegen::Codegen;
use momiji::runtime::Interpreter;
use momiji::{frontend, FrontendTiming};

#[derive(Parser)]
#[command(name = "momiji")]
#[command(about = "The Momiji programming language compiler", long_about = None)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile a Momiji source file to an executable
    Build {
        /// The source file to compile
        file: PathBuf,
        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Code generation backend
        #[arg(long, value_enum, default_value_t = Backend::Cranelift)]
        backend: Backend,
        /// Print phase timings for this command
        #[arg(long)]
        timings: bool,
    },
    /// Compile and run a Momiji source file
    Run {
        /// The source file to compile and run
        file: PathBuf,
        /// Execution backend
        #[arg(long, value_enum, default_value_t = Backend::Interp)]
        backend: Backend,
        /// Print phase timings for this command
        #[arg(long)]
        timings: bool,
    },
    /// Check a Momiji source file for errors without compiling
    Check {
        /// The source file or directory to check
        file: PathBuf,
        /// Skip frontend work if the file is unchanged since the last successful check
        #[arg(long)]
        changed: bool,
        /// Print phase timings for this command
        #[arg(long)]
        timings: bool,
    },
}

#[derive(Clone, Copy, Debug, ValueEnum, Eq, PartialEq)]
enum Backend {
    Cranelift,
    Interp,
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Build {
            file,
            output,
            backend,
            timings,
        } => build(&file, output.as_deref(), backend, timings),
        Commands::Run {
            file,
            backend,
            timings,
        } => run(&file, backend, timings),
        Commands::Check {
            file,
            changed,
            timings,
        } => momiji::check::check(&file, changed, timings),
    };

    if let Err(e) = result {
        eprintln!("{:?}", e);
        std::process::exit(1);
    }
}

fn build(
    file: &Path,
    output: Option<&Path>,
    backend: Backend,
    timings: bool,
) -> miette::Result<()> {
    let command_start = Instant::now();
    if backend == Backend::Interp {
        return Err(miette::miette!(
            "`build` does not support --backend interp; use `run --backend interp` instead"
        ));
    }

    let artifacts = frontend(file)?;
    if timings {
        print_frontend_timings("build", artifacts.timing);
    }
    let sem = artifacts.sem;

    // Code generation
    let codegen_start = Instant::now();
    let module_name = file.file_stem().unwrap().to_string_lossy();
    let mut codegen = Codegen::new(&module_name).map_err(|e| Report::new(e))?;

    codegen.compile(&sem).map_err(|e| Report::new(e))?;

    // Output object file
    let obj_path = file.with_extension("o");
    codegen
        .write_object(&obj_path, &sem)
        .map_err(|e| Report::new(e))?;
    let codegen = codegen_start.elapsed();

    let exe_path = output.map(PathBuf::from).unwrap_or_else(|| {
        let stem = file.file_stem().unwrap().to_string_lossy();
        PathBuf::from(stem.to_string())
    });

    // Link with system linker
    let link_start = Instant::now();
    link(&obj_path, &exe_path)?;
    let link = link_start.elapsed();

    // Clean up object file
    let _ = fs::remove_file(&obj_path);

    println!("Compiled to {}", exe_path.display());
    if timings {
        eprintln!(
            "timings build: frontend={}, codegen={}, link={}, total={}",
            fmt_duration(artifacts.timing.total()),
            fmt_duration(codegen),
            fmt_duration(link),
            fmt_duration(command_start.elapsed())
        );
    }

    Ok(())
}

fn run(file: &Path, backend: Backend, timings: bool) -> miette::Result<()> {
    let command_start = Instant::now();
    if backend == Backend::Interp {
        let artifacts = frontend(file)?;
        if timings {
            print_frontend_timings("run(interp)", artifacts.timing);
        }
        let exec_start = Instant::now();
        let mut interpreter = Interpreter::new(true);
        interpreter.run_program(&artifacts.sem).map_err(|e| {
            Report::new(e).with_source_code(NamedSource::new(artifacts.file_name, artifacts.source))
        })?;
        if timings {
            let exec = exec_start.elapsed();
            eprintln!(
                "timings run(interp): frontend={}, execute={}, total={}",
                fmt_duration(artifacts.timing.total()),
                fmt_duration(exec),
                fmt_duration(command_start.elapsed())
            );
        }
        return Ok(());
    }

    let temp_dir = std::env::temp_dir();
    let exe_name = file.file_stem().unwrap().to_string_lossy();
    let exe_path = temp_dir.join(format!("momiji_{}", exe_name));

    build(file, Some(&exe_path), Backend::Cranelift, timings)?;

    // Run the executable
    let exec_start = Instant::now();
    let status = Command::new(&exe_path)
        .status()
        .into_diagnostic()
        .wrap_err("Failed to run executable")?;
    let exec = exec_start.elapsed();

    // Clean up
    let _ = fs::remove_file(&exe_path);

    if timings {
        eprintln!(
            "timings run(cranelift): execute={}, total={}",
            fmt_duration(exec),
            fmt_duration(command_start.elapsed())
        );
    }

    if !status.success() {
        std::process::exit(status.code().unwrap_or(1));
    }

    Ok(())
}

fn link(obj_path: &Path, exe_path: &Path) -> miette::Result<()> {
    // Use clang as the linker for cross-platform compatibility
    let status = Command::new("clang")
        .arg(obj_path)
        .arg("-o")
        .arg(exe_path)
        .status()
        .into_diagnostic()
        .wrap_err("Failed to run linker (clang). Make sure clang is installed.")?;

    if !status.success() {
        return Err(miette::miette!("Linker failed with status: {}", status));
    }

    Ok(())
}

fn fmt_duration(duration: Duration) -> String {
    format!("{:.2}ms", duration.as_secs_f64() * 1000.0)
}

fn print_frontend_timings(command: &str, timing: FrontendTiming) {
    eprintln!(
        "timings {} frontend: read={}, lex={}, parse={}, sem={}",
        command,
        fmt_duration(timing.read),
        fmt_duration(timing.lex),
        fmt_duration(timing.parse),
        fmt_duration(timing.sem)
    );
}
