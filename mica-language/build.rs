use std::env;

fn enable_nan_boxing() -> bool {
    // NaN boxing is highly platform-specific, so we need to check if we're compiling for one of the
    // supported platforms before enabling it.
    env::var("CARGO_CFG_TARGET_ARCH").unwrap() == "x86_64"
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if enable_nan_boxing() {
        println!("cargo:rustc-cfg=mica_enable_nan_boxing");
    }
}
