# Spartan

An interpreter for the untyped lambda calculus. Input is parsed into an expression in the calculus (variable, abstraction, or application), and normalized through $\beta$-reduction. The substitution in reduction steps ensures that free variables are not caught by using explicit $\alpha$-conversion.

## What is the Lambda Calculus

- [Wikipedia](https://en.wikipedia.org/wiki/Lambda_calculus)
- [Stanford](https://plato.stanford.edu/entries/lambda-calculus/)

## Syntax

The interpreter parses input terms in a specific manner, to avoid ambiguity with associativity. Function applications must be surrounded by parentheses, for example $\lambda x.xy$ will result in a parsing error. This should instead be written as $\lambda x.(xy)$. This syntax follows the Backaus-Naur form (BNF) grammar given by [OpenDSA](https://opendsa-server.cs.vt.edu/OpenDSA/Books/PL/html/Syntax.html).

Function abstractions can be denoted either through the use of a double-backslash '\\', or with the unicode lowercase lambda character 'λ'.

## Implementation

Parsing is achieved through the use of monadic parser combinators, which are combined through the functions provided by the Applicative, Alternative, and Monad Haskell type-classes.
