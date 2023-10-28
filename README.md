# String parser Scala library

Inspired by 'Functional Programming in Scala, Paul Chiusano, Rúnar Bjarnason' book.

Purely functional library to parse and modify conveniently any text.

## Quick start

```scala
val input = "text45other6some"

val parser =
  lower *> digits(1, 2)
println(parser.run(input)) // "45"

val parser2 =
  lower *> digits(1, 2) *> lower ** digit
println(parser2.run(input)) // "other6"

val parser3 =
  lower *> digits(1, 2) *> lower ** digit *> lower
println(parser3.run(input)) // "some"
```
