package com.sparser.core

protected case class ParserOps[Parser[+_], A](p: Parser[A], pc: ParserCombinators[Parser]) {
  def |[B >: A](p2: Parser[B]): Parser[B] = pc.or(p, p2)

  def or[B >: A](p2: => Parser[B]): Parser[B] = pc.or(p, p2)

  def map[B](f: A => B): Parser[B] = pc.map(p)(f)

  def **(p2: Parser[A]): Seq[Parser[A]] = p2 :: p :: Nil

  def flatMap[B](f: A => Parser[B]): Parser[B] = pc.flatMap(p)(f)

  def *>[B](p2: => Parser[B]): Parser[B] = pc.skipLeft(p, p2)

  def <*(p2: => Parser[Any]): Parser[A] = pc.skipRight(p, p2)

  def times(n: Int): Parser[List[A]] = pc.times(n, p)

  def replicateM(n: Int): Parser[Seq[A]] =
    pc.replicateM(n, p)
}

protected case class ParserOpsSeq[Parser[+_], A](pl: Seq[Parser[A]], pc: ParserCombinators[Parser]) {
  def **(p2: Parser[A]): Seq[Parser[A]] = p2 +: pl

  def *>[B](p2: => Parser[B]): Parser[B] = pc.skipLeftSeq(pl, p2)
}
