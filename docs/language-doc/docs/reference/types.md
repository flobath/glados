# FunChill types

As of FunChill 2.0, the type system is kept very simple, featuring many integer types and a boolean type.

## Type table

Here is the full table of primary FunChill types:

| Type identifier       | Description                                           |
|-----------------------|-------------------------------------------------------|
| `i8`                  | 8  bits   signed integer                              |
| `i16`                 | 16 bits   signed integer                              |
| `i32`                 | 32 bits   signed integer                              |
| `i64`                 | 64 bits   signed integer                              |
| `u8`                  | 8  bits unsigned integer                              |
| `u16`                 | 16 bits unsigned integer                              |
| `u32`                 | 32 bits unsigned integer                              |
| `u64`                 | 64 bits unsigned integer                              |
| `bool`                | Boolean value (`true` / `false`)                      |
| `()`                  | Unit type (empty tuple)                               |
| `(t1, t2, t3, ...)`   | Tuple type (soon to come)                             |

## Details

### Integers

Integer types are described as short identifiers starting by either `u` or `i`, as in **u**nsigned integer or **i**nteger; the same as in Rust and Zig. Integer types can be manipulated by using arithmetic operators `+`, `-`, `*`, `/` and `%`.

### Booleans

The boolean type is special because it only has two possible values, which both are [keywords](./keywords.md), contrary to numbers which are literals. They are key in conditional expressions, being the only type that can express conditions. Boolean types can be manipulated by logical operator `&&` and `||`.

### Unit type

The unit type is what might be called the "void" type in other languages: a type that contains no information, and has a single variant. You will likely never to write this type by hand, since the only place where it can be used is in function return types, and it can be implicitly deduced if you simply [do not specify the return type](../introduction.md#functions).

Note that you can't declare variables of type `()`. Not that that it would be useful, anyways.

### Tuple type

The tuple type is meant to be a grouping of several values into a single object, which can be a first step towards defining your own data types. It is especially useful because it would allow functions to return several values at once.

However, tuples are not yet implemented in the language as of 2.0.

---

*[Back to reference index](./index.md)*
