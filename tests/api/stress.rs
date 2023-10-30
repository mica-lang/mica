use mica::{Engine, Value};

use super::RevealResultExt;

#[test]
fn gc_collect_should_not_cause_use_after_free() {
    let mut engine = Engine::new();
    let _: Value = engine.start("test.mi", "[]").reveal().trampoline().reveal();
    let _: Value = engine.start("test.mi", "[]").reveal().trampoline().reveal();
    let _: Value = engine
        .start("test.mi", "Gc.collect")
        .reveal()
        .trampoline()
        .reveal();
}
