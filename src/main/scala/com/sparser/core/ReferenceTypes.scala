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
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)

  def currentLine: String =
    if (input.length > 1)
      input.linesIterator.drop(line - 1).next()
    else ""

  def columnCaret = (" " * (col - 1)) + "^"
}

case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  /**
   * Display collapsed error stack - any adjacent stack elements with the
   * same location are combined on one line. For the bottommost error, we
   * display the full line, with a caret pointing to the column of the error.
   */
  override def toString: String =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
        context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)
}