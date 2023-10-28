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
    val input = "ab45za6as"

    val parser: Parser[String] = lower *> digits(1, 2)
    assert(parser.run(input) == "45")

    val parser2 = lower ** digits(1, 2) *> lower ** digit
    assert(parser2.run(input) == "za6")
  }

}
