package com.sparser.core

object ReferenceTypes {

  /** A parser is a kind of state action that can fail. */
  type Parser[+A] = ParseState => Result[A]

  /** `ParseState` wraps a `Location` and provides some extra
   * convenience functions. The sliceable parsers defined
   * in `Sliceable.scala` add an `isSliced` `Boolean` flag
   * to `ParseState`.
   */
  case class ParseState(location: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(location = location.copy(offset = location.offset + numChars))

    def input: String = location.input.substring(location.offset)

    def slice(n: Int): String = location.input.substring(location.offset, location.offset + n)
  }

  /* Likewise, we define a few helper functions on `Result`. */
  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e) => Left(e)
      case Success(a, _) => Right(a)
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }

    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }

  case class Success[+A](get: A, length: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
   * first index where the two strings differed. If s2 is
   * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(input: String, subStr: String, offset: Int): Int = {
    var i = 0
    while (i < input.length && i < subStr.length) {
      if (input.charAt(i + offset) != subStr.charAt(i)) return i
      i += 1
    }
    if (input.length - offset >= subStr.length) -1
    else input.length - offset
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(this, msg)

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)

  lazy val currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next() else ""

  def columnCaret: String = {
    val caretPosition = col - 1
    (" " * caretPosition) + "^"
  }
}

case class ParseError(location: Location, label: String) {
  def label(s: String): ParseError =
    copy(label = s)

  /** Display the error full line, with a caret pointing to the column of the error. */
  override def toString: String =
    s"""
       |Mismatch occurred: $label at line ${location.line} : column ${location.col}
       |${location.currentLine}
       |${location.columnCaret}
       |""".stripMargin
}
