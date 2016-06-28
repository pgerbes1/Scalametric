package com.scalametric.models.algebra

  trait Foldable[F[_]] {

	  def foldLeft[A, B](a: F[A])(init: B)(f: (B, A) => B): B

	  def foldRight[A, B](a: F[A])(init: B)(f: (A, B) => B): B

	  def foldMap[A, B](a: F[A])(f: A => B)(implicit mon: Monoid[B]): B =
		  foldLeft(a)(mon.empty)((b, a) => mon.add(b, f(a)))

	  def fold[A :  Monoid](a: F[A]): A = foldMap(a)(identity)

  }

  object Foldable {
	  def apply[F[_] : Foldable]: Foldable[F] = implicitly

	  def foldLeft[F[_] : Foldable, A, B](a: F[A])(init: B)(f: (B, A) => B): B =
		  implicitly[Foldable[F]].foldLeft(a)(init)(f)

	  def foldRight[F[_] : Foldable, A, B](a: F[A])(init: B)(f: (A, B) => B): B =
		  implicitly[Foldable[F]].foldRight(a)(init)(f)

	  def foldMap[F[_] : Foldable, A, B](a: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
		  implicitly[Foldable[F]].foldLeft(a)(mb.empty)((b, a) => mb.add(b, f(a)))

	  def fold[F[_] : Foldable, A : Monoid](a: F[A]): A = implicitly[Foldable[F]].foldMap(a)(identity)

	  implicit class foldableOps[F[_] : Foldable, A](a: F[A]) {
		  def foldLeft[B](init: B)(f: (B, A) => B): B = implicitly[Foldable[F]].foldLeft(a)(init)(f)

		  def foldRight[B](init: B)(f: (A, B) => B): B = implicitly[Foldable[F]].foldRight(a)(init)(f)

		  def foldMap[B](f: A => B)(implicit mon: Monoid[B]): B =
			  implicitly[Foldable[F]].foldLeft(a)(mon.empty)((b, a) => mon.add(b, f(a)))

		  def fold(implicit mon: Monoid[A]): A = implicitly[Foldable[F]].foldMap(a)(identity)
	  }

  }