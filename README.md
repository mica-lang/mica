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
