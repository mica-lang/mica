mod function_variants;

use std::process::Command;

use tracing::{info, info_span};

pub fn run() -> anyhow::Result<()> {
    let _span = info_span!("codegen").entered();

    let generated_files = [function_variants::generate()?];

    for path in &generated_files {
        let _span = info_span!("rustfmt", ?path).entered();
        let output = Command::new("rustfmt").arg(path).output()?;
        info!(code = ?output.status, "rustfmt done");
    }

    Ok(())
}
