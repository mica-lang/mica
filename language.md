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
```
nil       # Nil, means no value
true      # Boolean
false
1         # Number
1.41
"abc"     # String
"\"hi\""
```

#### Strings

Strings begin and end with double quotes, and can contain the following escape sequences:
- `\\` - literal backslash `\`
- `\"` - literal double quote `"`

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
```
> (a = 1) and (b = 2)
< 2

> a
(repl):1:1: error: variable 'a' does not exist
> b
(repl):1:1: error: variable 'b' does not exist
```

### Variables

Variables are assigned using the `=` operator:
```mica
> x = 1
< 1
```
The `=` operator returns the value of the variable. Combined with the fact that it's right- rather
than left-associative, this can be used for assigning the same value to multiple variables at once:
```mica
> x = y = 1
< 1

> x
< 1

> y
< 1
```
The REPL log above also shows that assigned variables can be referred to using bare identifiers.

Reading from an undefined variable is an error:
```mica
> swoosh
(repl):1:1: error: variable 'swoosh' does not exist
```

Variables can be reassigned:
```mica
> a = 1
< 1

> a
< 1

> a = 123
< 123

> a
< 123
```

#### Scope

Variables are subject to _scoping_. Mica has two kinds of scopes: global, and local.

The global scope is the default scope. A local scope can be introduced by using `do..end`.
```mica
do
   my_variable = 1
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
   my_variable = 1
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
x = Number.parse(readline())
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
if value = do_some_stuff() do
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
i = 1
while i <= 10 do
   print(i)
   i = i + 1
end
```

Just like in `if`, `while` introduces a new scope on the `while` keyword. This allows for creating
"iterators":
```
iterator = get_iterator_from_somewhere()
while i = iterator.next do
   print(i)
end
```

### `break` expressions

A `break` expression can be used to immediately jump past a loop.
```mica
i = 1
while true do
   print(i)
   i = i + 1
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
i = 1
print(while true do
   i = i + 1
   if i * i > 100 do
      break i
   end
end)  #> 11
```
In fact, a bare `break` is syntax sugar for `break nil`.

## Items

Items are a step above expressions, because they are treated specially by the compiler.
Each item introduces a new variable into scope.
Items are not expressions, but an expression can appear in any place an item can.
All items evaluate to `nil` when used as the last statement of a block.

The top level of a script, as well as any block such as `do..end`, is comprised of items.

### Function definitions

A function definition creates a new function and assigns it to a variable. The syntax is:
```mica
func name(param1, param2, param3)
   # body
end
```
This syntax is almost exactly the same as:
```mica
# Introduce the variable into scope first, so that the function can be called recursively.
name = nil
name = func (param1, param2, param3)
   # body
end
```
However, the `func name() end` form is preferred as it assigns a name to the function, which is
visible in stack traces. Anonymous functions have the name `<anonymous>`.

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

### Implementations

Implementations, or `impl` blocks, can be used to attach data and behavior to types.
```mica
impl SomeStructType
   # functions
end
```
An `impl` block can contain three types of functions: _static_ functions, _constructors_, and
_instance_ functions.

A static function is created by adding the `static` keyword after function parameters. Static
functions can be used as a way of putting functions into namespaces.
```mica
struct Greetings

impl Greetings
   func get(for_whom) static
      "Hello, ".cat(for_whom).cat("!")
   end
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
struct Vector

impl Vector
   func new(x, y) constructor
      # Declare fields that will store the X/Y coordinates of the vector.
      @x = x
      @y = y
   end

   func zero() constructor
      # Additional constructors must assign to the same set of fields as the first constructor.
      @x = 0
      @y = 0
   end

   # Now we can declare functions that operate on instances of the type.

   func len2()
      @x * @x + @y * @y
   end

   func len()
      # The `self` variable may be used to refer to the instance the function was called on, ie.
      # the left-hand side of the dot.
      self.len2.sqrt
   end
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

The implemented struct can be any expression, so nothing prevents you from doing this:
```mica
struct S
function obtain_struct()
   S
end

impl obtain_struct()
   # ...
end
```
If multiple `impl`s per type were allowed, the compiler would somehow need to keep track of what
fields each `impl` declares, which is impossible to do in a straightforward way due to the dynamic
type system.
