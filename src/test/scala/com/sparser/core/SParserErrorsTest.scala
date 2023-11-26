package com.sparser.core

import com.sparser.core.SParser._
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class SParserErrorsTest extends AnyFunSuite {
  test("error output should contain label") {
    val p = lower *> digits(1, 3)
    val expected =
      "Mismatch occurred: digits(1, 3) at line 1 : column 5\n" +
        "abcdBasdA\n" +
        "    ^"
    val err = intercept[SParseException] {
      p.run("abcdBasdA")
    }

    assert(err.message.contains(expected))
  }
}
