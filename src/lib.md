**Mica** is an embeddable scripting language for Rust.

- Features a **human-friendly** syntax inspired by Ruby and Lua.
- It's **simple** yet **extensible** by providing a small set of flexible features.
- It's **easily embeddable** into existing programs.

```text
## Hello, Mica!

struct Counter

impl Counter
   func new(start, increment) constructor
      @value = start
      @increment = increment
   end

   func value() @value end

   func increment()
      @value = @value + @increment
   end
end

c = Counter.new(1, 1)
while c.value < 100 do
   print(c.value)
   if c.value.mod(2) == 0 do
      print("even!")
   end
   c.increment()
end
```

# Getting started

Seeing you're here, you probably want to embed Mica into your own program. Worry not! There's an
easy-to-use, high-level API just waiting to be discovered by adventurous people like you.

To start out, build an [`Engine`] and load the [standard library][`std`] into it:
```rust
use mica::Engine;

let mut engine = Engine::new(mica::std::lib());
mica::std::load(&mut engine);
```

## Running code

Before we can run code, we must compile it into a [`Script`]. Note that compiling a script
mutably borrows your engine, and it cannot be used for anything else while a compiled script exists.
This is because a script must only ever be run in the engine it was compiled in.
Thus, the script mutably borrows the engine it was compiled by, so that you don't have to worry
about confusing two engines.
```rust
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
// The first argument passed is the file name, which is used in error messages.
let mut script = engine.compile("hello.mi", r#" print("Hello, world!") "#)?;
# Ok(())
# }
```
Now that you have a script, you can begin executing it by calling `script.start()`. This will start
up a new [`Fiber`], which represents a pausable thread of execution.<br>
<sup>Though currently there's no way to signal that you want to pause execution from within Mica.</sup>

```rust
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
# let mut script = engine.compile("hello.mi", r#" print("Hello, world!") "#)?;
let mut fiber = script.start();
# Ok(())
# }
```
A fiber doesn't start running immediately though. To make it start interpreting your bytecode, call
[`resume`][`Fiber::resume`]:
```rust
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
# let mut script = engine.compile("hello.mi", r#" print("Hello, world!") "#)?;
# let mut fiber = script.start();
use mica::Value;
while let Some(value) = fiber.resume::<Value>()? {
   println!("{value:?}");
}
// Output:
// Hello, world!
// nil
# Ok(())
# }
```
If you only care about the final value returned by the fiber, you can call
[`trampoline`][`Fiber::trampoline`] instead. The name comes from the fact that the function
_bounces_ into and out of the VM until execution is done.

Note that this function discards all intermediary values returned by the VM.
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
# let mut script = engine.compile("hello.mi", r#" print("Hello, world!") "#)?;
# let mut fiber = script.start();
let result: Value = fiber.trampoline()?;
println!("{result:?}");
# Ok(())
# }
```

## Working with values

As shown above, running a script produces one or more values. This is an important feature of Mica –
it's an _expression-oriented_ language. Everything you could possibly think of produces a value.

To make working with values less annoying, types that can be obtained from Mica's dynamically-typed
values implement the [`TryFromValue`] trait, and each function returning values from the VM
conveniently converts the returned value to a user-specified type.
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
// Tip: A fiber can be started immediately after compiling a script, by using Engine::start.
let mut fiber = engine.start("arithmetic.mi", r#" 2 + 2 * 2 "#)?;
let result: f64 = fiber.trampoline()?;
assert_eq!(result, 6.0);
# Ok(())
# }
```

Evaluating arbitrary code is fun, but it's not very useful unless we can give it some inputs from
our program. For that, we can use _globals_. Globals can be set using [`Engine::set`], and retrieved
using [`Engine::get`].
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
engine.set("x", 1.0f64);
engine.set("y", 2.0f64);
let result: f64 = engine.start("globals.mi", r#" x + y * 10 "#)?.trampoline()?;
assert_eq!(result, 21.0);
# Ok(())
# }
```
Scripts can also set globals, but this functionality is planned to be removed at some point in favor
of just returning values.
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
// The unit type implements TryFromValue that expects a nil value.
let _: () =
   engine.start(
      "code.mi",
      r#" x = 1
          nil "# // Explicitly return nil, because every assignment evaluates to its right-hand side
   )?
   .trampoline()?;
let x: f64 = engine.get("x")?;
assert_eq!(x, 1.0);
# Ok(())
# }
```

## Calling Rust from Mica

Mica wouldn't be an embeddable scripting language worth anybody's time if it didn't have a way of
calling Rust functions. To register a Rust function in the VM, [`Engine::add_function`] can be used:
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
engine.add_function("double", |x: f64| x * x)?;
assert_eq!(
   engine.start("ffi.mi", "double(2)")?.trampoline::<f64>()?,
   4.0
);
# Ok(())
# }
```
In the Mica VM Rust functions are named "foreign", because they are foreign to the VM. This
nomenclature is also used in this crate. Apart from this, Rust functions that are registered into
the global scope, and do not have a `self` parameter, are called _bare_ in the documentation.

Rust functions registered in the VM can also be fallible, and return a [`Result`]`<T, E>`.
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
// Note that the API also understands numeric types other than `f64`. However, the only type of
// numbers Mica supports natively is `f64`, so converting from bigger types incurs a precision loss.
engine.add_function("parse_integer_with_radix", |s: String, radix: u32| {
   usize::from_str_radix(&s, radix)
})?;
assert_eq!(
   engine.start("ffi.mi", r#" parse_integer_with_radix("FF", 16) "#)?.trampoline::<f64>()?,
   255.0
);
# Ok(())
# }
```
Calling a Rust function with the incorrect number of arguments, or incorrect argument types, will
raise a runtime error in the VM.
```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
# engine.add_function("parse_integer_with_radix", |s: String, radix: u32| {
#    usize::from_str_radix(&s, radix)
# })?;
assert!(engine.start("ffi.mi", r#" parse_integer_with_radix() "#)?.trampoline::<f64>().is_err());
assert!(engine.start("ffi.mi", r#" parse_integer_with_radix(1, 16) "#)?.trampoline::<f64>().is_err());
assert!(engine.start("ffi.mi", r#" parse_integer_with_radix("aa", 16) "#)?.trampoline::<f64>().is_ok());
# Ok(())
# }
```
Due to limitations of Rust's type system, strongly typed functions like the one in the example above
can only have up to 8 arguments. If more arguments, or a variable amount is needed, a function can
accept [`Arguments`] as its sole argument, and use it to process arguments.

Do note however that [`Arguments`]' API is comparatively low-level and will have you working with
[`RawValue`]s that are unsafe in many ways. If you really need that many arguments, maybe it's time
to rethink your APIs.

## Rust types in Mica

Mica allows for registering arbitrary user-defined types in the VM. As an example, let's implement
the `Counter` type from the example code at the top of the page, but in Rust.

First of all, for Rust type system reasons, your type needs to implement [`UserData`].

```rust
struct Counter {
   value: usize,
   increment: usize,
}

impl mica::UserData for Counter {}
```

With a type set up, you can then create a [`TypeBuilder`] and use it in [`Engine::add_type`].

```rust
# use mica::Value;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let mut engine = mica::Engine::new(mica::std::lib());
# mica::std::load(&mut engine);
# struct Counter {
#    value: usize,
#    increment: usize,
# }
# impl mica::UserData for Counter {}
use mica::TypeBuilder;

impl Counter {
   fn value(&self) -> usize { self.value }
   fn increment(&mut self) {
      self.value += self.increment;
   }
}

engine.add_type(
   // The argument passed to TypeBuilder::new is the name of the global that we want to bind the
   // type under.
   TypeBuilder::<Counter>::new("Counter")
      .add_constructor("new", |ctor| move |value: usize, increment: usize| {
         ctor.construct(Counter { value, increment })
      })
      .add_function("value", Counter::value)
      .add_function("increment", Counter::increment)
);

assert_eq!(
   engine
      .start("type.mi", r#"
         counter = Counter.new(10, 2)
         counter.increment()
         counter.value
      "#)?
      .trampoline::<f64>()?,
   12.0,
);
# Ok(())
# }
```
