package com.sparser.core

import scala.language.implicitConversions
import scala.util.matching.Regex

trait ParserCombinators[Parser[+_]]
  extends Monad[Parser]
    with Applicative[Parser] {
  self =>
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  val digit: Parser[String] = digits(1, 1)

  def digits(from: Int, to: Int): Parser[String] = (s"\\d{$from,$to}").r

  val digitsMany: Parser[String] = (s"\\d+").r

  val integer: Parser[Int] = digitsMany.map(_.toInt)

  val long: Parser[Long] = digitsMany.map(_.toLong)

  val whitespace: Parser[String] = "\\s*".r

  def skipLeft[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipLeftSeq[A, B](p: Seq[Parser[A]], p2: => Parser[B]): Parser[B] =
    map2(slice(sequence(p)), p2)((_, b) => b)

  def skipRight[B](p: Parser[B], p2: Parser[Any]): Parser[B] =
    map2(p, slice(p2))((a, _) => a)

  val lower: Parser[String] = ("\\p{javaLowerCase}+").r

  val upper: Parser[String] = ("\\p{javaUpperCase}+").r

  val anyNonDigit: Parser[String] = ("\\D*").r

  // implicits
  implicit def operators[A](p: Parser[A]): ParserOps[Parser, A] = ParserOps(p, this)

  implicit def operators[A](p: Seq[Parser[A]]): ParserOpsSeq[Parser, A] = ParserOpsSeq(p, this)

  implicit def string(s: String): Parser[String]

  implicit def toSeq[A](s: Parser[A]): Parser[Seq[A]] = s.map(Seq(_))

  implicit def regex(r: Regex): Parser[String]
}
