# Mica

[![Language reference](https://img.shields.io/badge/docs-language-blueviolet?style=flat-square)][langref]
[![Rust API](https://img.shields.io/badge/docs-API-blueviolet?style=flat-square)][rustapi]
[![GitHub](https://img.shields.io/badge/GitHub-mica--lang%2Fmica-blue?style=flat-square)][GitHub]
[![crates.io](https://img.shields.io/crates/v/mica?style=flat-square)][crates.io]

A simple, human-friendly scripting language, developed one feature at a time. Its goals are:

- **Human-friendly** syntax inspired by Ruby and Lua
- **Simple** and **familiar** to Rust developers feature-wise
- **Easily embeddable** into existing programs
- **Better performance** than most existing Rust scripting languages

```
# Hello, Mica!

struct Counter impl
    func new(start, increment) constructor = do
        @value = start
        @increment = increment
    end

    func value() = @value

    func increment() = do
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

At its current stage, it can be embedded into existing programs, but bugs may arise and certain
parts may be cumbersome to use. The performance is also not even close to where I'd like it to be.
But I want you to try it out and share your thoughts!

## Try it out

To compile Mica, use one of the following commands:
```sh
# To install the latest stable release:
$ cargo install mica-cli
# To compile the latest revision:
$ git clone https://github.com/mica-lang/mica
$ cd mica
$ cargo build -p mica-cli --release
```
Then you can try it out interactively, or run a file:
```sh
# To open the REPL:
$ mica
# To run a file:
$ mica filename.mi
```

Check out the [language reference][langref] for a detailed look at the language!

## Why?

The Rust ecosystem has plenty of existing scripting languages, but none of them quite cuts it for
me.
- [Lua](https://lua.org) is awesome, but I found [mlua](https://github.com/khvzak/mlua) and
  [rlua](https://github.com/amethyst/rlua) quite annoying to use due to their inability to bind
  most functions out of the box. You have to create wrapper functions that take giant tuples as
  arguments, which confuses the heck out of rustfmt, and is just plain inconvenient.
- [Rhai](https://github.com/rhaiscript/rhai) is nice feature-wise, but the AST-walk interpreter is
  very slow. A real waste of computer resources.
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
Sugary, but not quite sweet enough to give you instant diabetes. And handmade by myself.

Designing and implementing a programming language has been one of my arch nemeses for the
[past](https://github.com/liquidev/hayago) [few](https://github.com/liquidev/cflang) [years](https://github.com/liquidev/tsuki)
with varying levels of success, but I feel like finally, this is it. This time I'm gonna do it.



  [langref]: docs/language.md
  [rustapi]: https://docs.rs/mica/latest
  [GitHub]: https://github.com/mica-lang/mica
  [crates.io]: https://crates.io/crates/mica
