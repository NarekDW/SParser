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

  test("numbers test") {
    assert(integer.exec("42") == 42)
    assert(long.exec("4242424242421111234") == 4242424242421111234L)

    val combination = for {
      i <- integer
      _ = assert(i == 42)
      _ <- string(".")
      i2 <- integer
      _ = assert(i2 == 24)
      _ <- whitespace
      l <- long
      _ = assert(l == 4242424242421111234L)
    } yield ()
    combination.run("42.24 4242424242421111234")
  }
}
