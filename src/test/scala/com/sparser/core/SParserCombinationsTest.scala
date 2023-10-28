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

}
