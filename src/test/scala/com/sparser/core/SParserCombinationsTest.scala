package com.sparser.core

import org.scalatest.funsuite.AnyFunSuite
import SParser._
import ReferenceTypes.Parser

class SParserCombinationsTest extends AnyFunSuite {

  test("Combinations test **") {
    val parser =
      string("abc") **
        string("e") **
        digits(1, 2) **
        (string("f") | string("z"))

    val input = "abce45z"
    assert(parser.run(input) == input)
  }

  test("Combinations test *>") {
    val input = "text45other6some"

    val parser: Parser[String] = lower *> digits(1, 2)
    assert(parser.run(input) == "45")

    val parser2: Seq[Parser[String]] = parser *> lower ** digit
    assert(parser2.run(input) == "other6")

    val parser3: Parser[String] = parser2 *> lower
    assert(parser3.run(input) == "some")
  }

  test("Monad laws") {
    val p1 = digit
      .flatMap(n => string("a").replicate(n.toInt))
      .flatMap(x => string(x.mkString))
    val p2 = digit
      .flatMap(n => string("a").replicate(n.toInt)
        .flatMap(x => string(x.mkString))
      )

    val input = "2aaaa"
    val expected = "aa"
    assert(p1.run(input) == expected)
    assert(p2.run(input) == expected)

    val unit = SParser.succeed[String] _
    assert(digit.flatMap(unit).run("1") == "1")
    assert(unit("1").flatMap(_ => digit).run("1") == "1")
  }
}
