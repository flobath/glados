# FunChill expressions

In FunChill, the concept of an expression is rather simple: it refers to anything that has a value, such as numbers or variables. An important thing about expression is that they can always be assigned a type.

Expressions are a recursive data structure: many language constructs allow creating expressions from one or many other expressions. For example, when you write `1 + 2`, `1` is an [atomic expression](#atomic-expressions), same goes for `2`, and `1 + 2` as a whole is also an expression: an [operation expression](#operation-expressions)

## Different levels of expressions

In order to express things such as operator precedence or simply the way expressions are delimited, they are defined in different levels, and eventually recursing to the lowest level with parentheses wrapping expressions.

## Atomic expressions

Atomic expressions are the most basic building block for expressions. There are only three kinds of atomic expressions:

- Numeric literals (i.e. `123`)
- Boolean literals (`true` / `false`)
- Identifiers (names of variables or functions)

Numeric literals default to the `i32` type.  
Boolean literals of course have the `bool` type.  
Identifiers' type are known where the identifier has been declared.

## Primary expressions

Primary expressions are the basic structure from which expressions can be constructed recursively. There are three kinds of primary expressions:

- [Atomic expressions](#atomic-expressions) (of course atomic expressions can be used to construct higher order expressions)
- "Grouped" expressions: parenthesised [expressions](#general-expressions). This allows breaking operator precedence
- Block expressions: a language construct which wraps zero or more [statements](./statements.md) in an expression, which allows writing imperative code in conditional expressions for example

Grouped expressions have the type of the wrapped expressions.  
As of 2.0, block expressions have the [unit type `()`](./types.md#unit-type).

## Operation expressions

Operation expressions are the result of applying operators on [primary expressions](#primary-expressions).  
An application of zero operators on a primary expression also counts as a valid operation expression.

For a full list of operators, see the [operator table](./operator_table.md).

## Conditional expressions

Conditional expressions are the most basic control flow structure in FunChill. In total, 4 conditional expressions exist:

- `if`
- `if ... else`
- `unless`
- `unless ... else`

The expressions `if` and `unless` with no `else` are formed of 2 sub -expressions. `if ... else` and `unless ... else` are formed of 3 sub expressions.  
All sub-expressions part of conditional expressions can be [any type of expression](#general-expressions).

The expression `unless (A) ...` is just syntactic sugar for `if (!A) ...`. The condition is considered satisfied when it evaluates to `true` in the case of `if`, and when it evaluates to `false` in the case of `unless`.

All four conditional expressions expect their condition as a parenthesised expression of type 
`bool`.  
In the case of `if` / `unless` without an `else` arm, an expression of type `()` is expected after the condition. If and only if the condition is satisfied, then the expression will be evaluated (and may produce side effects), then the control flow will continue right after, no matter whether the condition has been satisfied. These conditionals will always evaluate to `()`.  
In the case of `if ... else` and `unless ... else`, a second expression is expected after the `else` block. Both expressions before and after the `else` must be of the same type. If the condition is satisfied, the first expression will be evaluated and the whole conditional will evaluate to its first arm. Otherwise, the second expression will be evaluted and the whole conditional will evaluate to its second arm.

## General expressions

"General" expressions are what is commonly referred to as "expressions" in the rest of this documentation. They can be either [operation expressions](#operation-expressions) or [conditional expressions](#conditional-expressions).

Note that since operations expressions are made of **zero** or more operations, an atomic expression such as `3` will indeed qualify as a "general" expression.

---

*[Back to reference index](./index.md)*
