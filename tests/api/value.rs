use mica::{Engine, Value};

use super::RevealResultExt;

#[test]
fn passing_tuples_to_mica() {
    let mut engine = Engine::new();

    engine.set("coords", (1, 2, 3)).reveal();
    let _: Value =
        engine.start("test.mi", "assert(coords == (1, 2, 3))").reveal().trampoline().reveal();
}

#[test]
fn receiving_tuples_from_mica() {
    let mut engine = Engine::new();

    let coords: (i32, i32, i32) =
        engine.start("test.mi", "(1, 2, 3)").reveal().trampoline().reveal();
    assert_eq!(coords, (1, 2, 3));
}
