package com.sparser.core

import SParser._
import org.scalatest.funsuite.AnyFunSuite

class SParserFunctionsTest extends AnyFunSuite {
  test("succeed should not depend on the input") {
    val succeedParser = SParser.succeed("1a2b")
    assert(succeedParser.run("abc123") == "1a2b")
    assert(succeedParser.run("xqew-1") == "1a2b")
  }

  test("Simple string parser should parse exact text") {
    val simpleStringParser = string("abc")
    assert(simpleStringParser.run("abc") == "abc")
    assert(simpleStringParser.run("abcdef123") == "abc")
  }

  test("Simple string parser should fail if text doesn't match") {
    val simpleStringParser = string("abc")
    assertThrows[RuntimeException](simpleStringParser.run("abdc"))
  }
}
