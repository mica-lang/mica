mod function_variants;

use tracing::info_span;

pub fn run() -> anyhow::Result<()> {
    let _span = info_span!("codegen").entered();

    function_variants::generate()?;

    Ok(())
}
