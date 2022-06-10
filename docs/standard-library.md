# The Mica programming language - standard library

## Overview

Mica has a fairly minimal standard library, whose parts can be enabled or disabled using Cargo
features.

Unfortunately I'm too lazy to write down all functions at the moment, and Mica doesn't have a
documentation generator yet; please browse the source code of `mica-std` for a list of available
functions.

- `Nil`: no methods
- `Boolean`: no methods
- [`Number`](../mica-std/src/builtins/number.rs)
- [`String`](../mica-std/src/builtins/string.rs)
- [`List`](../mica-std/src/builtins/list.rs)
- [`Dict`](../mica-std/src/builtins/dict.rs)
