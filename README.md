# String parser Scala library

Inspired by 'Functional Programming in Scala, Paul Chiusano, Rúnar Bjarnason' book.

Purely functional library to parse and modify conveniently any text.

## Quick start

```scala
val parser: Parser[String] =
  lower *> digits(1, 2)
val input = "abceasd45zasfdh"
parser.run(input) // "45"
```
