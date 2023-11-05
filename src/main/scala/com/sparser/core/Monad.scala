package com.sparser.core

trait Functor[F[_]] {
  def succeed[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def traverse[A, B](as: Seq[A])(f: A => F[B]): F[Seq[B]] =
    as.foldRight(succeed(Seq[B]()))((a, fbs) => map2(fbs, f(a))((seq, a) => a +: seq))

  def sequence[A](fas: Seq[F[A]]): F[Seq[A]] =
    traverse(fas)(identity)

  def replicate[A](n: Int, fa: F[A]): F[Seq[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)(_ -> _)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(
      apply(
        apply(succeed(f.curried))(fa)
      )(fb)
    )(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fde: F[D => E] = map3(fa, fb, fc)((a, b, c) => f.curried(a)(b)(c))
    apply(fde)(fd)
  }
}


trait Monad[F[_]] extends Functor[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => succeed(f(a)))

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
