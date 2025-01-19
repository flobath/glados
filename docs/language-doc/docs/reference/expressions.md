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

The expressions `if` and `unless` with no `else` are formed of 2 sub-expressions. `if ... else` and `unless ... else` are formed of 3 sub-expressions.  
All sub-expressions part of conditional expressions can be [any type of expression](#general-expressions).

The expression `unless (A) ...` is just syntactic sugar for `if (!A) ...`. The condition is considered satisfied when it evaluates to `true` in the case of `if`, and when it evaluates to `false` in the case of `unless`.

All four conditional expressions expect their condition as a parenthesised expression of type 
`bool`.  
In the case of `if` / `unless` without an `else` arm, an expression of type `()` is expected after the condition. If and only if the condition is satisfied, then the expression will be evaluated (and may produce side effects), then the control flow will continue right after, no matter whether the condition has been satisfied. These conditionals will always evaluate to `()`.  
In the case of `if ... else` and `unless ... else`, a second expression is expected after the `else` block. Both expressions before and after the `else` must be of the same type. If the condition is satisfied, the first expression will be evaluated and the whole conditional will evaluate to its first arm. Otherwise, the second expression will be evaluted and the whole conditional will evaluate to its second arm.

## Conditional loops

Conditional loops are a simple control flow structure in FunChill, some of them are someway similar to conditional expressions. In total, 4 conditional loops exist:

- `while`
- `until`
- `do ... while`
- `do ... until`

The expressions `while` and `until` with no `do` are formed of 2 sub-expressions. `do ... while` and `do ... until` are also formed of 2 sub-expressions but they evaluate them in the reverse order.
All sub-expressions part of conditional loops can be [any type of expression](#general-expressions).

The expression `until (A) ...` is just syntactic sugar for `while (!A) ...`. The condition is considered satisfied when it evaluates to `true` in the case of `while`, and when it evaluates to `false` in the case of `until`.

All four conditional loops expect their condition as a parenthesised expression of type `bool`.
In the case of `while` / `until`, an expression of type `()` is expected after the condition. If and only if the condition is satisfied, then the expression will be evaluated (and may produce side effects), then the control flow will re-evaluate the whole loop until the condition is no longer satisfied.
In the case of `do ... while` and `do ... until`, a `do` keyword followed by an expression of type `()` is expected before the condition. The expression is evaluated before the control flow evaluates the condition. As for `while` / `until` loops, if the condition is evaluated to `true` / `false`, the whole loop is re-evaluated.
After a conditional loop ends, the control flow continue to evaluate the next instructions.

## Iterative loops

Iterative loops are a complex control flow structure in FunChill with a behavior similar to conditional loops. Only one iterative loops exists:

- `for ... in`

The expression `for ... in` is formed of 2 [statements](./statements.md#list-of-statements) and 1 sub-expression: a variable declaration, a range definition (defined as `(a,b)`), an [expression of any type](#general-expressions).

The `for ... in` loop first declares a locally scoped integer variable and assigns it to the first value of the range. The control flow then evaluates the sub-expression and increment the local variable of `1`. The control flow re-evaluate the loop (except the variable declaration) until the variable becomes equal to or greater than the last value of the range.
After an iterative loop ends, the control flow continue as for conditional loops.

## General expressions

"General" expressions are what is commonly referred to as "expressions" in the rest of this documentation. They can be either [operation expressions](#operation-expressions) or [conditional expressions](#conditional-expressions).

Note that since operations expressions are made of **zero** or more operations, an atomic expression such as `3` will indeed qualify as a "general" expression.

---

*[Back to reference index](./index.md)*
