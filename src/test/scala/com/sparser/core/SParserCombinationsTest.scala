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
    val parser: Parser[String] = lower *> digits(1, 2)
    val input = "abceasd45zasfdh"
    assert(parser.run(input) == "45")
  }

}
