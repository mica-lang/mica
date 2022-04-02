# Mica

A simple, human-friendly scripting language, developed one feature at a time.

- **Human-friendly** syntax inspired by Ruby and Lua
- **Simple** yet **extensible** by providing a small amount of flexible language features
- **Easily embeddable** into existing programs
- Still highly work in progress!

You can try out the REPL with `cargo run -p mica-cli --release`.

A language reference is available [here](language.md).

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
> hyperfine --warmup 5 'target/release/mica code/functions.mi' 'lua5.1 code/functions.lua'
Benchmark 1: target/release/mica code/functions.mi
  Time (mean ± σ):      93.2 ms ±   0.6 ms    [User: 92.1 ms, System: 1.2 ms]
  Range (min … max):    92.0 ms …  95.5 ms    31 runs

Benchmark 2: lua5.1 code/functions.lua
  Time (mean ± σ):      27.4 ms ±   0.2 ms    [User: 26.1 ms, System: 1.5 ms]
  Range (min … max):    27.0 ms …  28.5 ms    98 runs

Summary
  'lua5.1 code/functions.lua' ran
    3.40 ± 0.04 times faster than 'target/release/mica code/functions.mi'
```
It's not ideal yet but hopefully it'll get better with time. Current bottlenecks include:
- A really stupid compiler that does zero optimizations
- Stack-based rather than register-based virtual machine
- Rust's safety features working against my goal of being faster than Lua
