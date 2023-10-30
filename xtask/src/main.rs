use clap::{Parser, Subcommand};
use tracing::{error, metadata::LevelFilter};
use tracing_subscriber::{prelude::*, EnvFilter};

mod codegen;

#[derive(Parser)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Generate code that is checked into the repository.
    Codegen,
}

fn main() {
    let subscriber = tracing_subscriber::registry()
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::DEBUG.into())
                .from_env_lossy(),
        )
        .with(tracing_subscriber::fmt::layer().without_time());
    tracing::subscriber::set_global_default(subscriber)
        .expect("cannot set default tracing subscriber");

    let args = Args::parse();

    let result = match args.command {
        Command::Codegen => codegen::run(),
    };
    if let Err(err) = result {
        error!(%err, "xtask failed");
    }
}
