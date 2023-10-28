package com.sparser.core

import scala.language.implicitConversions
import scala.util.matching.Regex

trait ParserCombinators[Parser[+_]] {
  self =>
  def succeed[A](a: A): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(x => succeed(f(x)))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  val digit: Parser[String] = digits(1, 1)

  def digits(from: Int, to: Int): Parser[String] = (s"\\d{$from,$to}").r

  val digitsMany: Parser[String] = (s"\\d+").r

  val whitespace: Parser[String] = "\\s*".r

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  def skipLeft[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  // TODO: move to Monad
  def sequence[A](lma: Seq[Parser[A]]): Parser[Seq[A]] =
    lma match {
      case x :: xs => flatMap(sequence(xs))(l => map(x)(_ +: l))
      case Nil => succeed(Nil)
    }

  def skipLeftL[B](p: Parser[Any], p2: => Seq[Parser[B]]): Parser[Seq[B]] =
    map2(slice(p), sequence(p2))((_, b) => b)

  def skipRight[B](p: Parser[B], p2: Parser[Any]): Parser[B] =
    map2(p, slice(p2))((a, _) => a)

  val lower: Parser[String] = ("\\p{javaLowerCase}+").r

  val upper: Parser[String] = ("\\p{javaUpperCase}+").r

  val anyNonDigit: Parser[String] = ("\\D*").r

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _) else succeed(Nil)

  def times[A](n: Int, p: Parser[A]): Parser[List[A]] =
    listOfN(n, p)

  // implicits
  implicit def operators[A](p: Parser[A]): ParserOps[Parser, A] = ParserOps(p, this)

  implicit def operators[A](p: Seq[Parser[A]]): ParserOpsSeq[Parser, A] = ParserOpsSeq(p, this)

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]
}
