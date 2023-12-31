package com.sparser.core

import ReferenceTypes._

import scala.language.implicitConversions
import scala.util.matching.Regex

object SParser extends ParserCombinators[Parser] {
  sp =>
  override def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(a, n) => f(a)(s.advanceBy(n)).advanceSuccess(n)
      case f: Failure => f
    }

  def map2[A, B, C](fa: Parser[A], fb: Parser[B])(f: (A, B) => C): Parser[C] =
    fa.flatMap(a => fb.map(b => f(a, b)))

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = s =>
    s1(s) match {
      case Failure(_) => s2(s)
      case r => r
    }

  override implicit def string(word: String): Parser[String] = {
    s => {
      val i = firstNonmatchingIndex(s.location.input, word, s.location.offset)
      if (i == -1)
        Success(word, word.length)
      else
        Failure(s.location.advanceBy(i).toError(s"'$word'"))
    }
  }

  override implicit def regex(r: Regex): Parser[String] = {
    s =>
      r.findPrefixOf(s.input) match {
        case None => Failure(s.location.toError(s"regex $r"))
        case Some(m) => Success(m, m.length)
      }
  }

  override def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_, n) => Success(s.slice(n), n)
      case f: Failure => f
    }

  override def withLabel[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def either[A](p: Seq[Parser[A]])(s: String): Either[ParseError, String] =
    exec(p)(s)(_.extract.map(_.mkString))

  def list[A](p: Seq[Parser[A]])(s: String): List[A] =
    exec(p)(s) {
      _.extract match {
        case Left(parseError: ParseError) => throw new RuntimeException(parseError.toString)
        case Right(list) => list
      }
    }

  def run[A](p: Seq[Parser[A]])(s: String)(separator: String): String =
    exec(p)(s) {
      _.extract match {
        case Left(parseError: ParseError) => throw SParseException(parseError.toString)
        case Right(list) => list.mkString(separator)
      }
    }

  def exec[A, R](p: Seq[Parser[A]])(s: String)(comb: Result[List[A]] => R): R = {
    val s0 = ParseState(Location(s))
    val f = p.foldLeft(succeed(List.empty[A]))((b, a) => a.flatMap(aa => b.map(x => aa :: x)))
    comb(f(s0))
  }

  implicit def execOps[A](parsers: Seq[Parser[A]]): ParserExecutionOps[A] = new ParserExecutionOps(parsers)

  implicit def execOpsStr[A](parser: Parser[A]): ParserExecutionOpsStr[A] = new ParserExecutionOpsStr(parser)

  class ParserExecutionOps[A](parsers: Seq[Parser[A]]) {
    def either(input: String): Either[ParseError, String] = sp.either(parsers)(input)

    def list(input: String): List[A] = sp.list(parsers)(input)

    def run(input: String, separator: String = ""): String = sp.run(parsers)(input)(separator)
  }

  class ParserExecutionOpsStr[A](parser: Parser[A]) extends ParserExecutionOps(parsers = Seq(parser)) {
    def exec(input: String): A = sp.exec(Seq(parser))(input)(_.extract.map(_.head)) match {
      case Left(parseError: ParseError) => throw new RuntimeException(parseError.toString)
      case Right(a) => a
    }
  }
}

