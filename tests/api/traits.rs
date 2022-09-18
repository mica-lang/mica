use mica::Value;

use crate::api::RevealResultExt;

use super::create_engine;

#[test]
fn exposing_traits_from_rust() {
   let mut engine = create_engine();

   let mut builder = engine.build_trait("GameLoop").reveal();
   let m_draw = builder.add_function("draw", 1).reveal();
   let m_update = builder.add_function("update", 0).reveal();
   let game_loop_trait = builder.build();
   engine.set("GameLoop", game_loop_trait).reveal();

   let game: Value = engine
      .start(
         "test.mi",
         include_str!("traits/exposing_traits_from_rust.mi"),
      )
      .reveal()
      .trampoline()
      .reveal();

   let _: Value = engine.call_method(game.clone(), m_draw, [Value::from(0.1666)]).reveal();
   let _: Value = engine.call_method(game.clone(), m_update, []).reveal();

   let did_draw: bool = engine.call_method(game.clone(), ("did_draw", 0), []).reveal();
   let did_update: bool = engine.call_method(game, ("did_update", 0), []).reveal();

   assert!(did_draw);
   assert!(did_update);
}
