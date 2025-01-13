# FunChill statements

In FunChill, statements are a language construct which expresses imperative code, contrary to [expressions](./expressions.md), which are only about combining values together.

## List of statements

As of FunChill 2.0, only 4 four statements can be found in the language:

- `return` statements, which stop execution of the current function and optionally specify a return value
- Variable declaration statements: introduce a new variable into scope
- Assignment statements: change the value of a variable
- [Simple expressions](./expressions.md#general-expressions)

## Where statements can be found

Statements are found between `{`curly braces`}`, separated by *[end of statements](#end-of-statements)* (semicolons or linebreaks).

The most common occurrence is of obvious: functions! A function body is nothing more than a [block expression](./expressions.md#primary-expressions), thus is made of statements, like variable declarations, variable assignments, and `return` statements.

The reason why expressions are also statements is to allow an imperative style in functions, for example calling other functions without retrieving their return value:

```
fun do_stuff {
    do_thing1()
    do_thing2()
    do_thing3()
}
```

And of course, since block expressions are first class expressions, this means it is also possible to write statements only executed conditionally, by using [conditional expressions](./expressions.md#conditional-expressions):

```
fun do_smart_stuff(i32 x) {
    if (x > 7) {
        i32 a = do_thing1() + x % 2

        use_thing1(a)
    }
    use_thing2(x)
}
```

## End of statements

In FunChill, statements written in block expressions must be terminated by an end of statement. There are two kinds of end of statements:

- Semicolon `;`
- Linebreak `\n` or `\r\n`

Strictly speaking, end of statements are part of statements themselves; as such, when we say that block expressions contain zero ore more statements, we imply that and end of statement must be present for each statement, including the last one.

Linebreaks and semicolons can be used interchangeably as end of statements, even in the same block expression:
```
fun f {
    i32 a = 15 // linebreak ends the statement

    a = 2; // semicolon ends the statement
    a = 3; a = 4; a = 5 // Several statements can be written in
                        // in one line with semicolons
}
```

The fact that linebreaks are valid end of statements aims at making semicolons optional, so feel free to use this more idiomatic style when writing FunChill instead of cramming all your end of lines with semicolons.

Making linebreaks valid end of statements comes a cost, though: it makes splitting expressions on several lines harder, because the linebreak could be interpreted as an end of statement. To help mitigate that, FunChill gives special power to certain characters: discard any number of following linebreaks to allow multiline expressions. Those characters are:

- `,` Comma
- `(` Opening parenthesis
- `{` Opening brace
- And all infix and prefix operators

Linebreaks are also discarded in many places in conditional expressions, as described in the [whitespace reference](./whitespace.md). In fact, you can more generally find good information and insight about how linebreaks can work as end of statement that reference.

---

*[Back to reference index](./index.md)*
