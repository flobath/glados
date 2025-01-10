# FunChill operator precedence table

Here is a table of all the available operators in FunChill, along with their associativity and precedence.  
Note that all suffix operators have the same precedence, and same goes for all prefix operators. The most precedent are suffix operators, then prefix (such that `!myfunc()` first applies function call and then negates the results), and then infix operators have different levels of precedence.

|Precedence | Operator  | Type  | Description               | Associativity |
|-----------|-----------|-------|---------------------------|---------------|
| 1         |`(expr...)`|Suffix | Function call             | Left-to-right |
| 2         | `-`       |Prefix | Arithmetic opposite       | Right-to-left |
| 2         | `+`       |Prefix | Arithmetic plus sign      | Right-to-left |
| 2         | `!`       |Prefix | Logical not operator      | Right-to-left |
| 3         | `*`       |Infix  | Multiplication            | Left-to-right |
| 3         | `/`       |Infix  | Division                  | Left-to-right |
| 3         | `%`       |Infix  | Modulus                   | Left-to-right |
| 4         | `+`       |Infix  | Addition                  | Left-to-right |
| 4         | `-`       |Infix  | Subtraction               | Left-to-right |
| 5         | `>`       |Infix  | Greater than              | Left-to-right |
| 5         | `<`       |Infix  | Lesser  than              | Left-to-right |
| 5         | `>=`      |Infix  | Greater than or equal     | Left-to-right |
| 5         | `<=`      |Infix  | Lesser  than or equal     | Left-to-right |
| 6         | `==`      |Infix  | Equals                    | Left-to-right |
| 6         | `!=`      |Infix  | Not equals                | Left-to-right |
| 6         | `&&`      |Infix  | Logical AND               | Left-to-right |
| 6         | `||`      |Infix  | Logical OR                | Left-to-right |

---

*[Back to reference index](./index.md)*
