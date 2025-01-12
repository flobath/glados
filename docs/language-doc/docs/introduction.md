# Introduction to FunChill

The FunChill programming language is a C-like language which aims at being simple to read and write while providing sensible features.

## Quick example

Here is a simple factorial function written in FunChill.

```
fun my_factorial(i32 n): i32 {
    if (n < 0) {
        return -1 // -1 is returned as an error
    }
    if (n <= 1) {
        return 1
    }
    return n * my_factorial(n - 1)
}
```

As you can see from this example, FunChill is very similar to modern C-like programming languages so it should be simple to learn both for beginners or experienced developers.

Some key features in this code snippet are:

- `fun` is used to introduce a function declaration
- `:` declares the function return type
- Semicolons at line endings are *[optional](./link/to/explanation/of/end-of-statements)*

## Overview of features

This overview aims at giving you an idea of what's available in the language and how to do basic things. For more detail about a specific topic, follow the links from the overview; or for a complete overview of what's available, have a look at the [language reference pages](./reference/index.md).

### Built-in types

Here is a table of the built-in types:

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
| `(type1, type2, ...)` | [Tuple type](./types.md#tuple) (group of values)      |
| `()`                  | Unit type (empty tuple)                               |

### Operators

FunChill features all basic arithmetic and logical operators you would expect from any programming language. Here is a short list of all operators:

- Prefix `+` / `-`: Sign prefixes, so that you can write signed number literals (`-75`, `+3`)
- Prefix `!`: Logical not operator
- All arithmetic `+` `-` `*` `/` `%`: For addition, subtraction, multiplication, division and modulus.
- Comparison operators: `>` `>=` `<` `<=`
- Equality operators: `==` and `!=`
- Boolean logical operators: `&&` and `||`

Addition and subtraction have lower precedence than the others, so `1 + 2 * 3` is indeed parsed as `1 + (2 * 3)`.  
For a complete reference of operators associativity and precedence, see the full [operator table](./reference/operator_table.md).

### Functions

Functions can only be declared in the global scope. A function declaration is introduced by the `fun` keyword, followed by the function identifier, the parameter list, the return type and the function body.
Here is a simple example:
```
fun my_add(i32 a, i32 b): i32 {
    return a + b
}
```

A function body consists of zero or more statements wrapped in curly braces. In general, a statement can be a variable declaration, a function call or a `return` statement for example. See the complete list of statements [here]().

If you have a function which returns `()`, you can omit the return type:
```
// Explicit return type of `()`
fun my_function(i32 i): () {}

// Implicit return type of `()`
fun my_function(i32 i) {}
```

If your function takes no parameters, you can omit the parameter list:
```
// Explicit empty parameter list
fun my_get_int(): i32 {
    return 42
}

// Omitted parameter list
fun my_get_int: i32 {
    return 42
}
```

It is also possible to omit both the parameter list and the return type:
```
fun f {}
```

#### Main function

To declare the main function to your program, use the `main` keyword, followed by a parameter list, a return type, and a function body.

Everything that can be omitted in a function declaration can also be omitted in a main declaration.

```
main(): i32 {
    return 42
}
```

### Comments

Comments are just what you expect them to be. Use `//` to introduce a line comment: all characters until the end of line will be ignored.
```
// I am a comment
fun f { // Comment after opening brace
    // Comment inside a function
    i32 a = 0 // Comment after a variable
// Weirdly indented comment
} // Comment after closing brace
```

You can also use `/*` and `*/` to introduce a block comment − that is − to ignore all characters between the opening `/*` and the closing `*/`.  
As of 2.0, nested block comments are not supported, such that `/* /* /* */` represents a single, syntactically valid block comment, while `/* /* */ */` represents a block comment followed by a trailing `*/` which will certainly cause a parsing error.

```
/* This is a block comment */
fun /*inline block comment*/ myfunc/* abc */(i32 a /*, i32 b*/)/*: ()*/
/*
{
    return
}
*/
{
    some_other_func(a)
}
```
