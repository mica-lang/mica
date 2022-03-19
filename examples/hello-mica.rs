//! An example of how to evaluate code using Mica.

use mica::Engine;

fn main() {
   let engine = Engine::new(mica::std::lib());
   mica::std::load(&engine).unwrap();

   engine.set("x", 123_i32).unwrap();
   engine.set("y", 456_i32).unwrap();
   let result: i32 = engine.start("<main>", "x * 3 + y").unwrap().trampoline().unwrap();
   println!("{result}");
}
