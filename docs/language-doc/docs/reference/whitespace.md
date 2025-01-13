# Whitespace in FunChill

In FunChill, whitespace is mostly ignored. This means that you are free to indent your code however you like, or add any amount of whitespace in between every word.

## Characters counted as "whitespace"

As of FunChill 2.0, the only characters recognised as pure whitespace are:

- ASCII Space ` ` (0x20)
- ASCII Tab   `\t` (0x09)

Importantly, linebreaks are not considered simple whitespace because they have an important role in functions: they can be used interchangeably with semicolons to end statements.

## What happens to whitespace in code

Whitespace is handled by the FunChill lexer, which explicitly ignores them and produces no token when lexing whitespace. Whitespace still serves as a delimiter between identifiers, so it is not always optional.

A simple example where whitespace is mandatory:
```
// This works
fun myfunc {}

// This doesn't work
funmyfunc {}
```

## What happens to linebreaks

In FunChill, two linebreak sequences are recognised:

- ASCII LineFeed `\n` (0x0a)
- ASCII CarriageReturn LineFeed `\r\n` (0X0D0A)

These linebreak sequences are exceptionally ignored by the lexer when preceded by a backslash `\` (0x5C), in which case they behave as any whitespace.  
So, in the following example, a backslash allows writing the addition on a separate line:
```
fun f {
    i32 a = 1 \
        + 2
}
```

So, finally, what happens to linebreaks which are not escaped?  
In short, when outside of a function: they are simply ignored; when inside a function body: they are an alternative to semicolons as "end of statements".

... But they still sometimes ignored for readability. For a technical explanation of how linebreaks can be ignored within a function body, keep reading, or for a practical explanation, go to the [statement reference](./statements.md#end-of-statements).

### How linebreaks are conditionally ignored

To understand the following explanation, you will need a basic understanding of how the FunChill compiler works. There are two parts of the compiler which are relevant to understand how linebreaks are ignored:

1. The Lexer, which converts source files into a stream of *tokens* (identifiers, literals, operators, brackets, ...)
2. The Parser, which converts a stream of tokens into an **a**bstract **s**yntax **t**ree (usually referred to as AST): a recursive data structure which expresses the syntax of the language

The FunChill lexer usually converts linebreak sequences (`\n` or `\r\n`) to `Linebreak` tokens, which will be included in the token stream resulting from lexing a file.

Escaped linebreaks are ignored at the lexer level: when a linebreak sequence is preceded by the backslash character, it will be explicitly ignored, thus producing no token in the resulting token stream.

On the other hand, linebreaks which are not escaped make it to the parser, which implements some smart rules to allow linebreaks to be an alternative to semicolons while also allowing the user to split expressions into several lines.

To start with, line breaks are ignored at the top level (outside of function bodies).  
Within a function body, line breaks are explicitly not ignored: they are preserved preciously when parsing expressions or operator chains so that they can be consumed as an "end of statement" at the end. This means the following:

```
fun f {
    i32 a = 8
          + 2
}
```

will be parsed the same as
```
fun f {
    i32 a = 8;
    +2;
}
```

But thankfully, the following parses properly
```
fun f {
    i32 a = 8 +
            2
}
```

How so?, well, some specific tokens discard all linebreaks directly following them. It's actually quite a big number of tokens; here is the full list:

- `;`
- `:`
- `,`
- `{`
- `(`
- All prefix and infix operators

Linebreaks are also discarded between the different parts of a conditional expression:

- Between the `if`/`unless` and the parenthesised condition
- Between the condition and the first arm expression
- Between the first arm expression and the `else` (only if there is an `else`)
- Between the `else` and the second arm expression

such that the following are correctly parsed as single conditional expressions:
```
fun f {
    // Same as if (a) b else c
    if
        (a)
        b
    else
        c

    // Same as above
    if (a)
        b
    else
        c

    // Same as if (a) b
    if (a)
        b
}
```
