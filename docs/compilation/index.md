# FunChill compilation process

The FunChill compiler is a program which converts FunChill source code into FunChill executables.

It is implemented in several modules which, chained together, make up the full compiler. Each module converts a certain representation of FunChill code into another, eventually leading from plain text source code to binary object code.

The different modules are:
- The [lexer](#lexer)
- The [parser](#parser)
- The [compiler](#compiler)
- The [serializer](#serializer)

## Flow chart of intermediate representations and modules

```
  +-------------+
  | Source code |
  +-------------+
         |
         | Lexer
         V
  +--------------+
  | Token stream |
  +--------------+
         |
         | Parser
         V
      +-----+
      | AST |
      +-----+
         |
         | Compiler
         V
+------------------+
| Instruction list |
+------------------+
         |
         | Serializer
         V
    +----------+
    | Bytecode |
    +----------+
```

## Lexer

The Lexer takes as input the raw, plain text source code, and converts it into a stream of tokens. The set of tokens is defined in src/Tokens.hs, and includes for example:

- All keywords
- Opening/Closing Parentheses/Braces
- Literals
- Operators

Along with each token is also kept its position in the source file, which helps error reporting in further compilation steps.

The FunChill lexer is implemented using the Alex lexer generator.

## Parser

The Parser takes as input the stream of tokens constructed by the lexer and converts into an Abstract Syntax Tree. This compilation step is key because it is where we convert from a flat list (token stream) to a recursive data structure which more accurately represents our language.

The parser may report parsing errors when the input is invalid, which are properly reported with source file name, line and column of the error

The AST data structure definition can be found in src/Parser/AST.hs

The FunChill parser is implemented using megaparsec parser combinators.

## Compiler

Note: in this paragraph, the word "compiler" is used to describe the FunChill compiler module which converts an AST into an instruction list. This is not to be mistaken with the FunChill compiler as a whole, which contains several modules.

The Compiler is where source code becomes "alive": it takes as input an abstract syntax tree and outputs a list of instructions which contains all compiled functions.  
The FunChill instruction set aims at being executed by a stack machine, which the kind of VM we implemented. The instruction set is described at the top of src/StackMachine.hs

The compiler features several passes, in which it desugars some syntactic forms, checks types, and finally produces an instruction list.

The compiler is implemented by hand, with no major external dependency.

## Serializer

The FunChill serializer is a rather small module which simply encodes an instruction list in our custom bytecode, which is a compact format for storing instruction lists on disk.

The serializer is implemented using the bytestring `Put` monad, which helps handling binary output in a semi-imperative way.
