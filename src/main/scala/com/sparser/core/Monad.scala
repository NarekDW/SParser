package com.sparser.core

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def succeed[A](a: A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => succeed(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: Seq[F[A]]): F[Seq[A]] =
    lma match {
      case x :: xs => flatMap(sequence(xs))(l => map(x)(_ +: l))
      case Nil => succeed(Nil)
    }

  def traverse[A, B](la: Seq[A])(f: A => F[B]): F[Seq[B]] = la match {
    case x :: xs => flatMap(traverse(xs)(f))(a => map(f(x))(_ +: a))
    case Nil => succeed(Nil)
  }

  def replicateM[A](n: Int, ma: F[A]): F[Seq[A]] =
    sequence(List.fill(n)(ma))

  def filterM[A](ms: Seq[A])(f: A => F[Boolean]): F[Seq[A]] = {
    ms match {
      case x :: xs => flatMap(f(x))(b => map(filterM(xs)(f))(l => if (b) x +: l else l))
      case Nil => succeed(Nil)
    }
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)
}

