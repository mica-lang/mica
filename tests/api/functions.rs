//! Tests around binding functions to the VM.

use mica::{Engine, TryFromValue, TypeBuilder, UserData, Value};

use super::RevealResultExt;

#[derive(Debug, Clone, PartialEq)]
struct Vec2 {
    x: f32,
    y: f32,
}

impl UserData for Vec2 {}

const fn _assert_implements_try_from_value<T>()
where
    T: TryFromValue,
{
}
const _: () = {
    _assert_implements_try_from_value::<Vec2>();
};

#[test]
fn user_data_parameters_can_be_passed_into_functions() {
    let mut engine = Engine::new();

    engine
        .add_type(TypeBuilder::<Vec2>::new("Vec2").add_static("new", |x, y| Vec2 { x, y }))
        .reveal();
    engine
        .add_function("same_vec2", |v: Vec2, x: f32, y: f32| assert_eq!(v, Vec2 { x, y }))
        .reveal();

    let _: Value =
        engine.start("test.mi", "same_vec2(Vec2.new(1, 2), 1, 2)").reveal().trampoline().reveal();
}

#[test]
fn user_data_parameters_cause_type_errors() {
    let mut engine = Engine::new();

    engine.add_type(TypeBuilder::<Vec2>::new("Vec2")).reveal();
    engine.add_function("eat_vec2", |_: Vec2| ()).reveal();

    let result: Result<Value, _> = engine.start("test.mi", "eat_vec2(1)").reveal().trampoline();
    assert!(result.is_err());
}

#[test]
fn unregistered_user_data_parameters_cause_type_errors() {
    let mut engine = Engine::new();

    engine.add_function("eat_vec2", |_: Vec2| ()).reveal();

    let result: Result<Value, _> = engine.start("test.mi", "eat_vec2(1)").reveal().trampoline();
    assert!(result.is_err());
}

#[test]
fn mutable_aliasing_of_user_data_is_not_allowed() {
    let mut engine = Engine::new();

    engine
        .add_type(
            TypeBuilder::<Vec2>::new("Vec2")
                .add_static("new", |x, y| Vec2 { x, y })
                .add_function("mutably_alias", |_: &mut Vec2, _: Vec2| ()),
        )
        .reveal();

    let result: Result<Value, _> = engine
        .start(
            "test.mi",
            r#"
                v = Vec2.new(0, 0)
                v.mutably_alias(v)
            "#,
        )
        .reveal()
        .trampoline();
    assert!(result.is_err());
}
