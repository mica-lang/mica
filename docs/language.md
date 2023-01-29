# The Mica programming language - reference manual

## Overview

Mica is a dynamically typed scripting language akin to Lua and Ruby. The main goal of Mica is to
have a small but flexible feature set, packed up in a human-friendly syntax.

## Comments

Comments can be used for annotating source code with human-readable info. A comment begins with the
hash `#` symbol, and ends at the end of a line.
```
# This is a comment.
# Hello!
1 + 2
```

## Expressions

At the core of everything in Mica are expressions. Each expression produces a value whose type is
determined at runtime.

### Literals

Literals are a way of inputting values directly into a program's source code. A literal expression
evaluates to the value of the literal.
```mica
nil       # Nil, means no value
true      # Boolean
false
1         # Number
1.41
1e9
1_000
"abc"     # String
"\"hi\""
\r"C:\Windows\System32"
[]        # List
[1, 2]
[:]       # Dict
["a": 3]
```

#### Numbers

Number literals are composed of a series of decimal digits, optionally followed by a decimal point
`.`, which must then be followed by another series of decimal digits which are fractional part.
As a special rule, the `.` can also be immediately followed by a starting character of an
[identifier](#identifiers), which is parsed as a call to a method on the number.

This can then be followed by an `e` or `E`, signifying scientific notation, and the `e` must be
followed by an optional `+` or `-`, signifying the sign of the exponent, and digits signifying the
exponent.

Digits (decimal or not) can be separated with underscores `_`.

Examples of valid numbers showcasing the various features include:
```mica
1000
1_000
3.14159265
0.000_000_001
3e2     # 300
3.14e2  # 314
1e1     # 10
1e+1
1_e+1
5e-2    # 0.05
```
Be aware of some gotchas.
```mica
# The following example is *not* a number, but a method call (0)._000
0._000
# The following example is invalid syntax, because the exponent must have at least one digit.
1e_
```

Mica also has syntax sugar for **32-bit** integer literals with an arbitrary radix. This syntax is
`\radix:value`, for instance `\16:DEADBEEF` or `\8:777`.
The character set used is decimal digits from 0 to 9, and letters from A to Z, in that order.
Lowercase and uppercase letters are allowed and equivalent. Because of the size of this character
set, the maximal allowed radix is 36 (and the minimal is 2).

As with any numbers, underscore separators are permitted between all digits.
```
\16:DEADBEEF
# same as
\1_6:Dead_beef
```

There exist a few shorthands for commonly used radixes.
- `\b110` or `\B110` - same as `\2:110`
- `\o777` - same as `\8:777`
    - Note that an uppercase O is *not* permitted, because it's easily confused with a zero.
- `\xFF` or `\XFF` - same as `\16:FF`

Integers that out of the 32-bit range are invalid, though this limitation may be relaxed in
the future. The current rationale for imposing such a limit is that bit operations in the standard
library only operate on 32 bits.

#### Strings

Strings begin and end with double quotes, and can contain the following escape sequences:
- `\\` - literal backslash `\`
- `\'` - literal apostrophe `'`
- `\"` - literal quote `"`
- `\n` - line feed, ASCII 0Ah
- `\r` - carriage return, ASCII 0Dh
- `\t` - tabulator, ASCII 09h
- `\u{x}` - Unicode [scalar value](https://www.unicode.org/glossary/#unicode_scalar_value)
    - Between braces must be a hexadecimal digit <= 10FFFFh not contained in the range D800h–DFFFh (inclusive).
    - Like in any number, digits can be separated with underscores.
    - At least one digit must be present.

Raw strings begin with the extended literal sequence `\r`, followed by double quotes, any sequence
of characters that doesn't contain double quotes, and end with double quotes. Raw strings do not
interpret any escape sequences. This also means that raw strings themselves cannot contain
quotes `"`, though this restriction may get lifted at some point in the future.

Note that ordinary _and_ long string literals must not contain embedded line breaks.

Mica also features a literal for getting the numeric value of any Unicode codepoint.
```mica
\u' '  # \x20
\u'🗿'  # \x1F5FF
```

Strings that span multiple lines are a little hard to represent using the usual double-quoted `""`
syntax, which is why _long_ string literals exist. These literals allow you to more conveniently
represent multiline content.
To construct a long string literal, prefix each line of the string with `\\`.
```mica
# Spaces after \\ are not stripped.
let s =
    \\Hello,
    \\ world!
assert(s == "Hello,\n world!")
```

#### Lists

Lists are a data type for storing values in a sequence. Their literals open and close with square
brackets `[]`, and contain comma-separated values.
```mica
let cool_languages = ["Rust", "Mica", "Lua"]
```
The list stored in the variable `cool_languages` holds three strings, but Mica lists can store any
data type. They are heterogenous, which means that multiple data types can be stored in the same
list - including other lists.
```mica
let identity_mat4 = [
    [1, 0, 0, 0],
    [0, 1, 0, 0],
    [0, 0, 1, 0],
    [0, 0, 0, 1],  # trailing comma is optional
]
```
Elements can be retrieved using the `get/1` function, and modified using the `set/2` function.
```mica
let numbers = [1, 2, 3]
assert(numbers.get(0) == 1)  # elements are indexed starting from 0
numbers.set(0, 2)
assert(numbers == [2, 2, 3])
```
More functions, for eg. appending and removing elements from lists, can be found in the
[standard library documentation](./standard-library.md).

Lists are passed by reference, but compared by value. This means that a list copied into two
separate variables refers to the same data store, but comparing independent lists always compares
them by individual elements.
```mica
let a = []
let b = a
assert(a == b)
assert(a.is_empty)
a.push(1)
assert(a == b)
# Note that [1] creates a *new* list with a completely new data store, containing 1.
assert(a == [1] and b == [1])
```

#### Dicts

Dicts, short for *dict*ionaries, are a data type for storing values associatively - they map one
value (the key) to another value, just like real world dictionaries map words from one language to
another language. Dict literals are enclosed in square brackets `[]` just like list literals, but
instead of bare elements they use pairs of values separated by a colon `:`. The empty list is
written as `[:]`.

```mica
let dependencies = [
    "rust": "1.61",
    "mica": "0.3.0",
]
```
Elements can be retrieved using `get/1`.
```mica
assert(dependencies.get("rust") == "1.61")
```
Elements can be inserted or overwritten using `insert/2` – which returns the old value, or `nil` if
there wasn't any value stored previously – and removed using `remove/1`, which returns the removed
value.
```mica
dependencies.insert("lua", "5.4")
assert(dependencies.insert("mica", "0.4.0") == "0.3.0")
assert(dependencies.remove("lua") == "5.4")
```

Just like lists, dicts are heterogenous. Any value can be used as a key or a value - even a dict
itself.
```mica
let weird = [
    "#ffffff": "white",
    [1, 2]: 0,
    [3, 4]: 1,
    ["x": 3]: 5,
]
assert(weird.get([1, 2]) == 0)
assert(weird.get(["x": 3]) == 5)
```

Just like lists, dicts are passed by reference and compared by value.
```
let a = [1: 1]
let b = a
a.insert(1, 2)
assert(a == b)
assert(a == [1: 2])
```

### Identifiers

Identifiers allow for referring to existing, named values.
```
snake_case
PascalCase
zażółć_gęślą_jaźń
```
An identifier must start with an alphabetic character or an underscore, and continues with zero or
more alphanumeric characters or underscores. Alphabetic and alphanumeric characters are defined in
[Chapter 4 of the Unicode Standard][unicode-chapter4].

    [unicode-chapter4]: https://www.unicode.org/versions/Unicode14.0.0/ch04.pdf

The naming conventions used in Mica code should be `PascalCase` for type names and `snake_case` for
everything else (variables, functions). `SCREAMING_SNAKE_CASE` can be used for constants, however
the immutability of such values is not enforced by the language.

Certain identifiers are reserved as keywords. This means they have a significant meaning in the
language syntax and cannot be used as ordinary values.

### Operators

Mica defines the following operators, grouped by precedence (largest to smallest):
```
@ (prefix)
. ()
! (prefix)  - (prefix)
*  /
+  -
==  !=  <  >  <=  >=
=
and
or
```

#### Arithmetic

The operators `+`, `-` (both prefix and infix), `*`, `/` are used for arithmetic and perform
addition, subtraction, multiplication, and division respectively.

```mica
> 1 + 1
< 2

> 1 - 1
< 0

> 8 * 8
< 64

> 64 / 8
< 8
```

The prefix `-` can be used to negate numbers.

```
> -(1 + 2)
< -3
```

#### Relation

The operators `==`, `!=`, `<`, `>`, `<=`, `>=` can be used for comparing objects for equality or
order. Each of these operators returns a `Boolean`.

Ordered relation between values of distinct types is undefined and raises a runtime error.

```mica
> 1 == 1
< true

> 2 != 1
< true

> 3 < 3
< false

> 3 <= 3
< false

> true < 1
error: type mismatch, expected Boolean but got Number
     (repl):1:6  <main>
```

#### Logic

The operators `!` (prefix), `and`, and `or` perform the logic operations NOT, AND, and OR
respectively.

These operators work on values of _all_ types and their results depend on whether a value is
_truthy_ or _falsy_. Falsy values include `nil` and `false`. All other values are truthy.
Truthiness of values is used to determine how a value would convert to a `Boolean`, without actually
performing a conversion.

```mica
> !true
< false

> !false
< true

> false and 1
< false

> 1 and 2
< 1

> nil or 2
< 2

> 1 or 2
< 1
```

The REPL log above shows a property of the `and` and `or` operators, which is called
_short-circuiting_. If the result of an operation can be deduced from only evaluating the left
operand, the right operand will not be evaluated and instead the left one will be returned.

`and` and `or` introduce a new [scope](#scope), which means that although you can declare variables
inside them, you will not be able to refer to them outside:
```mica
> (let a = 1) and (let b = 2)
< 2

> a
(repl):1:1: error: variable 'a' does not exist
> b
(repl):1:1: error: variable 'b' does not exist
```

#### Function calls

The `()` infix operator is used for calling [functions](#function-definitions). The left-hand side
of the operator is the function that should be called, and the right hand side is the list of
arguments to pass to the function. Inside the called function, each argument is bound to a
variable named after the parameter at the same position.

`print` is a built-in function that echoes its arguments to stdout:
```mica
> print("Hello!")
Hello
< nil
```

The `.` infix operator is used for calling functions that are bound to values. The left hand side
of the operator is the _receiver_, and the right-hand side is the name of the function to call.
Additional arguments may be provided by following the name of the function up with `()` containing
a list of arguments.

```mica
> 4.sqrt      # Call without arguments
< 2

> 3.hypot(4)  # Call with one argument
< 4
```

See [implementations](#implementations) for information on how to declare functions bound to values.

### Variables

Mica separates defining a variable from assigning to it. A variable can be introduced into scope
by using the `let` expression:
```mica
> let x = 1
< 1
```
As shown above, `let` returns the value. This can be useful when used in conjunction with `if` to
check for `nil` values, forming a construct akin to Rust's `if let`. It can also be used to create
multiple variables with the same value:
```mica
> let x = let y = 1
< 1

> x
< 1

> y
< 1
```

Variables can be shadowed, even in the same scope. Do keep in mind that shadowing a variable very
far from its declaration can get quite unreadable; therefore try to limit its usage to sequences
of transformations.
```mica
let s = obtain_string()
let s = do_something_with_string(s)
let s = finalize(s)
print(s)
```

Variables are assigned using the `=` operator:
```mica
> x = 1
< 1
```

Reading from an undefined variable is an error:
```mica
> swoosh
(repl):1:1: error: variable 'swoosh' does not exist
```

#### Scope

Variables are subject to _scoping_. Mica has two kinds of scopes: global, and local.

The global scope is the default scope. A local scope can be introduced by using `do..end`.
```mica
do
    let my_variable = 1
    print(my_variable)  #> 1
end
```
A `do..end` block returns the value of the last expression inside.

The only semantic difference between the two is that global variables are _persistent_. This means
that a global variable is never deleted, and is always reachable.

Local variables on the other hand, are temporary, and are deleted as soon as the block they were
declared in `end`s.
```mica
> do
    let my_variable = 1
end
< 1

> my_variable
(repl):1:1: error: variable 'my_variable' does not exist
```

### `if` expressions

`if` expressions allow for evaluating different _branches_ of code based upon _conditions_.
```mica
if condition do
    # branch
end
```
The condition can be any expression. The branch will execute only if the condition is evaluated to
be truthy. Once the branch is finished executing, no other conditions nor branches will be evaluated.
Otherwise evaluation will jump over the branch, over to the next condition, until the end of the
`if` expression is reached.

The return value of an `if` is the last expression evaluated inside a branch. If no branch is
evaluated, the return value is `nil`.

More branches can be specified by using the `elif` keyword:
```mica
# readline function provided by host program
let x = Number.parse(readline())
if x == 1 do
    "one!"
elif x == 2 do
    "two!"
elif x == 3 do
    "three!"
end
```
A fallback branch can be specified by using the `else` keyword:
```mica
if readline() == "yes" do
    print("Continuing.")
else
    print("Cancelling...")
end
```

Each `if` expression branch introduces a new scope that begins on the keyword that begins the
branch. This means that variables can be declared inside the branch, which allows for easy `nil`
checks.
```
if let value = do_some_stuff() do
    # value is guaranteed to be non-nil
    value.do_something(123)
end
```

### `while` loops

`while` is an expression that can be used for looping.
```mica
while condition do
    # body
end
```
The condition will be evaluated, and if found truthy, the body will execute. Once the body is done
executing, evaluation will jump back to the condition. If the condition is falsy, the entire loop
will be jumped over.

By default, the result value of a `while` loop is `nil`.

A basic loop that counts up from 1 to 10:
```mica
let i = 1
while i <= 10 do
    print(i)
    i = i + 1
end
```

Just like in `if`, `while` introduces a new scope on the `while` keyword. This allows for creating
"iterators":
```
let iterator = get_iterator_from_somewhere()
while i = iterator.next do
    print(i)
end
```

### `for` loops

`for` is a loop expression similar to `while`, but instead of operating on a boolean expression
it allows for iterating over an _iterator_. The iterator is any value that implements the `Iterator`
[trait](#trait-definitions), which is defined as follows:
```mica
trait Iterator
    func has_next()
    func next()
end
```
A `for` loop is desugared to a `while` loop as follows:
```mica
for binding in iter do
    # body
end

# becomes

do
    # This _iterator variable is hidden by the compiler, and cannot be referred to.
    let _iterator = iter
    # It's also worth noting that these method calls do not involve global lookups to resolve
    # Iterator, as they would in normal Mica code.
    while Iterator.has_next(_iterator) do
        let binding = Iterator.next(_iterator)
        do
            # body
        end
    end
end
```

### `break` expressions

A `break` expression can be used to immediately jump past a loop.
```mica
let i = 1
while true do
    print(i)
    let i = i + 1
    if i * i >= 100 do
        break
    end
end
print("done!")
```
In the above example, once the `break` expression is hit as a result of the `if` condition being
truthy, execution will jump past the loop onto the line with `print`.

`break` can also be used to override the default `nil` return value of a loop:
```mica
# Find the first number whose square is greater than 100.
let i = 1
print(while true do
    i = i + 1
    if i * i > 100 do
        break i
    end
end)  #> 11
```
In fact, a bare `break` is syntax sugar for `break nil`.

### Function definitions

A function definition creates a new function and assigns it to a variable. The syntax is:
```mica
func name(param1, param2, param3) = expression
```
This syntax is almost exactly the same as:
```mica
# Introduce the variable into scope first, so that the function can be called recursively.
let name = nil
name = func (param1, param2, param3) = expression
```
However, the `func name() = expression` form is preferred as it assigns a name to the function,
which is visible in stack traces. Anonymous functions have the name `<anonymous>`.

To create a multiline function, a `do` block can be used as the expression:
```mica
func say_nice_things() = do
    print("Hey!")
    print("You look great today!")
end
```

### Struct definitions

A struct definition creates a new user-defined _type_.
```mica
struct Example
print(Example)  #> <[type Example]>
```
Every declared type is unique, and equal only to itself:
```mica
struct Example
struct Another
assert(Example == Example)
assert(Another == Another)
assert(Example != Another)
```
The `struct T` expression returns the newly created struct after introducing it into scope.
```
assert(struct Example == Example)
```

### Implementations

Implementations, or `impl` blocks, can be used to attach data and behavior to types.
```mica
SomeStructType impl
    # functions
end
```
An `impl` block can contain three types of functions: _static_ functions, _constructors_, and
_instance_ functions.

A static function is created by adding the `static` keyword after function parameters. Static
functions can be used as a way of putting functions into namespaces.
```mica
# Thanks to `impl`'s infix position, it can be chained naturally after a struct declaration.
struct Greetings impl
    func get(for_whom) static =
        "Hello, ".cat(for_whom).cat("!")
end

assert(Greetings.get("world") == "Hello, world!")
```

A constructor is created by adding the `constructor` keyword after function parameters.
The role of a constructor is to create an _instance_ of a type. Unlike the type itself, each
instance can have data attached to it, by using _fields_. Fields work very much like variables,
albeit they use different syntax: each field is comprised of the `@` symbol, followed by the field's
name, eg. `@greeting`.

The first declared constructor is the only place where new fields can be declared. Any additional
constructors or functions afterwards must only ever refer to fields declared in the first
constructor. Additionally, each constructor after the first one must assign to all fields that were
declared in the first one.

```mica
struct Vector impl
    func new(x, y) constructor = do
        # Declare fields that will store the X/Y coordinates of the vector.
        @x = x
        @y = y
    end

    func zero() constructor = do
        # Additional constructors must assign to the same set of fields as the first constructor.
        @x = 0
        @y = 0
    end

    # Now we can declare functions that operate on instances of the type.

    func len2() =
        @x * @x + @y * @y

    func len() =
        # The `self` variable may be used to refer to the instance the function was called on, ie.
        # the left-hand side of the dot.
        self.len2.sqrt
end

v = Vector.new(3, 4)
assert(v.len == 5)
```

As previously mentioned, there's a `self` variable in instance functions; the same variable is also
available in constructors and static functions, albeit with different meanings:
- In constructors, `self` refers to the newly created instance of the type.
- In instance functions, `self` refers to the _receiver_, that is, the instance the function was
  called on.
- In static functions, `self` refers to the type itself.

After `impl` is used on a type, that type becomes _sealed_, which means that it cannot be
implemented anymore. This prevents monkey-patching foreign types, which is often considered bad
programming practice, though the actual reason behind sealing has more to do with how dynamic
Mica's `impl` blocks are.

If multiple `impl`s per type were allowed, the compiler would somehow need to keep track of what
fields each `impl` declares, which is impossible to do in a straightforward way due to the dynamic
type system.

The implemented struct can be any expression, so nothing prevents you from doing this:
```mica
struct S
func obtain_struct() =
    S

obtain_struct() impl
    # ...
end
```

Apart from this, an `impl` block returns the implemented struct, so eg. returning a newly
implemented struct from a function is possible.
```mica
func make_me_a_struct() =
    struct TheStruct impl
        func say_hi() static =
            print("Hi!!")
    end

let AStruct = make_me_a_struct()
AStruct.say_hi()

let Another = make_me_a_struct()
assert(Another != AStruct)
```

### Trait definitions

Traits allow for defining list of functions a type must implement. These functions are namespaced
separately from other functions defined outside of traits, so it's possible to have two traits
that define a function `example`, and no conflicts will occur between them.

This feature is commonly known as _interfaces_ or _protocols_ in other programming languages.

To define a trait, the `trait` keyword is used:
```mica
trait MyTrait
    func do_something()  # note the lack of the equals sign '='
end
```
Implementing a trait is done through introducing an `as` block inside an `impl`:
```mica
struct MyImplementer impl
    func new() constructor = nil

    as MyTrait
        func do_something() = do
            print("Hello!")
        end
    end
end
```
Note that all trait methods are _instance_ methods. Allowing static methods in traits or static
implementations of traits wouldn't make that much sense, because Mica is a dynamically typed
language - _everything is a value._

To call a trait method, the usual `receiver.do_something()` syntax cannot be used, because it would
be ambiguous - instead, the trait must be used as a "relay":
```mica
let receiver = MyImplementer.new()
MyTrait.do_something(receiver)  # the first argument becomes `self`
```
