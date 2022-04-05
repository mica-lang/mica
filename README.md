# Mica

[Language reference][langref] · [Rust API][rustapi] · ![Crate version](https://img.shields.io/crates/v/mica?style=flat-square)

A simple, human-friendly scripting language, developed one feature at a time.

- **Human-friendly** syntax inspired by Ruby and Lua
- **Simple** yet **extensible** by providing a small amount of flexible language features
- **Easily embeddable** into existing programs
- Still highly work in progress!

You can try out the REPL with `cargo run -p mica-cli --release`.

## Try it out

To compile Mica, use the following command:
```sh
$ cargo build -p mica-cli --release
# or, if you want to run it immediately:
$ cargo run -p mica-cli --release
```

Check out the [language reference][langref] to have a detailed look at the language!

## Performance

```
func factorial(n)
   i = 1
   x = 1
   while i <= n do
      x = x * i
      i = i + 1
   end
   x
end

iteration = 1
result = 0
while iteration <= 100000 do
   result = factorial(15)
   iteration = iteration + 1
end

print(result)
```
<p align="center">Factorial benchmark</p>

```
> hyperfine --warmup 5 'target/release/mica code/functions.mi' 'lua5.1 code/functions.lua' 'python3 code/functions.py'
Benchmark 1: target/release/mica code/functions.mi
  Time (mean ± σ):      94.8 ms ±   6.8 ms    [User: 93.8 ms, System: 1.2 ms]
  Range (min … max):    91.5 ms … 121.1 ms    31 runs

Benchmark 2: lua5.1 code/functions.lua
  Time (mean ± σ):      27.5 ms ±   1.9 ms    [User: 26.7 ms, System: 1.2 ms]
  Range (min … max):    26.9 ms …  45.9 ms    97 runs

Benchmark 3: python3 code/functions.py
  Time (mean ± σ):     117.5 ms ±   1.3 ms    [User: 116.2 ms, System: 1.4 ms]
  Range (min … max):   115.6 ms … 119.9 ms    25 runs

Summary
  'lua5.1 code/functions.lua' ran
    3.44 ± 0.34 times faster than 'target/release/mica code/functions.mi'
    4.27 ± 0.30 times faster than 'python3 code/functions.py'
```
It's not ideal yet but hopefully it'll get better with time. Current bottlenecks include:
- Stack-based rather than register-based virtual machine
- Rust not having a way of doing direct threading in the interpreter dispatch loop. _Tail calls when_
- The bytecode compiler not optimizing much

## Why?

The Rust ecosystem has plenty of existing scripting languages, but none of them quite cuts it for
me.
- [Lua](https://lua.org) is awesome, but I found [mlua](https://github.com/khvzak/mlua) and
  [rlua](https://github.com/amethyst/rlua) quite annoying to use due to their inability to bind
  most functions out of the box. You have to create wrapper functions that take giant tuples as
  arguments, which confuses the heck out of rustfmt, and is just plain inconvenient.
- [Rhai](https://github.com/rhaiscript/rhai) is nice, but the AST-walk interpreter is very slow.
  A waste of computer resources which I cannot afford to have in my programs.
- [Dyon](https://github.com/pistondevelopers/dyon)… exists, but it seems more like an
  experimenting ground for implementing language features rather than a language designed for end
  users.
- [Rune](https://github.com/rune-rs/rune) is by far the most promising, but even the author admits
  that performance isn't the primary goal right now.
- And there's plenty more languages, though these seem like the most prominent ones as far as I
  could tell. I also ruled out weird and wacky (including [FP](https://en.wikipedia.org/wiki/Functional_programming))
  languages, because due to current processor architectures they're doomed to remain merely
  research projects. Also, imperative languages are the most widely taught ones by far.

There's also a number of unfinished crates with bindings for more niche scripting languages that
are written in C, but, well, _they're unfinished_.

I wanted a language that would take performance seriously. Be designed with specific goals in mind.
Sugary, but not quite sweet enough to give you instant diabetes. And made by me.

Designing and implementing a programming language has been one of my arch nemeses for the
[past](https://github.com/liquidev/hayago) [few](https://github.com/liquidev/cflang) [years](https://github.com/liquidev/tsuki)
with varying levels of success, but I feel like finally, this is it. This time I'm gonna do it.



  [langref]: docs/language.md
  [rustapi]: https://docs.rs/mica/latest
