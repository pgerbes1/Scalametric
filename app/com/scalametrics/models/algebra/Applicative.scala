package  com.scalametrics.models.algebra

import scala.languageFeature.higherKinds

  trait Applicative[M[_]] extends Functor[M] {
	  def pure[A](v: A): M[A]
	  def <*>[A, B](a: M[A])(f: M[A => B]): M[B]
	  def <@>[A, B, C](f: (A, B) => C)(a: M[A])( b: M[B]): M[C] = <*>(b)(<*>(a)(pure(f.curried)))
	  def <@@>[A, B, C, D](f: (A, B, C) => D)(a: M[A])(b: M[B])(c: M[C]): M[D] = <*>(c)(<*>(b)(<*>(a)(pure(f.curried))))
  }
  object Applicative {
	  def apply[M[_] : Applicative]: Applicative[M] = implicitly

	  def pure[A, M[_] : Applicative](v: A): M[A] = implicitly[Applicative[M]].pure(v)

	  def <*>[M[_] : Applicative, A, B](a: M[A])(f: M[A => B]): M[B] = implicitly[Applicative[M]].<*>(a)(f)

	  def <@>[M[_], A, B, C](f: (A, B) => C)(a: M[A])( b: M[B])(implicit applic: Applicative[M]): M[C] = {
		  applic.<*>(b)(applic.<*>(a)(applic.pure(f.curried)))
	  }

	  def <@@>[M[_], A, B, C, D](f: (A, B, C) => D)(a: M[A])( b: M[B])(c: M[C])(implicit applic: Applicative[M]): M[D] = {
		  applic.<*>(c)(applic.<*>(b)(applic.<*>(a)(applic.pure(f.curried))))
	  }

	  implicit val listApplicative: Applicative[List] = new Applicative[List] {
		  def pure[A](v: A): List[A] = List(v)

		  def fmap[A, B](l: List[A])(f: A => B): List[B] = {
			  l.map(f(_))
		  }

		  def <*>[A, B](l: List[A])(f: List[A => B]): List[B] = {
			  l.flatMap { a =>
				  f.map(b => b(a))
			  }
		  }
	  }

	  implicit val optionApplicative = new Applicative[Option] {
		  def pure[A](v: A): Option[A] = Option(v)

		  def fmap[A, B](o: Option[A])(f: A => B): Option[B] = o match {
			  case Some(_) => Option(f(o.get))
			  case None => None
		  }
		  def <*>[A, B](o: Option[A])(f: Option[A => B]): Option[B] = o match {
			  case Some(_) => Option(f.get(o.get))
			  case None => None
		  }
	  }
	  implicit class ApplicativeOps[A, M[_] : Applicative](a: M[A]) {
		  def pure(v: A): M[A] = implicitly[Applicative[M]].pure(v)

		  def <*>[B](f: M[A => B]): M[B] = implicitly[Applicative[M]].<*>(a)(f)

		  def <@>[B, C](f: (A, B) => C)(b: M[B])(implicit applic: Applicative[M]) = {
			  applic.<*>(b)(applic.<*>(a)(applic.pure(f.curried)))
		  }

		  def <@@>[B, C, D](f: (A, B, C) => D)(b: M[B])(c: M[C])(implicit applic: Applicative[M]) = {
			  applic.<*>(c)(applic.<*>(b)(applic.<*>(a)(applic.pure(f.curried))))
		  }
	  }
  }

  class ApplicativeSemigroup[A, M[_]](implicit ap: Applicative[M], sg: Semigroup[A]) extends Semigroup[M[A]] {
	  def add(l: M[A], r: M[A]) = ap.<@>(sg.add)(l)(r)
  }

  class ApplicativeMonoid[A, M[_]](implicit app: Applicative[M], mon: Monoid[A]) extends ApplicativeSemigroup[A, M]
	  with Monoid[M[A]] {
	  lazy val empty = app.pure(mon.empty)
  }

  class ApplicativeGroup[A, M[_]](implicit app: Applicative[M], grp: Group[A]) extends ApplicativeMonoid[A, M]
	  with Group[M[A]] {
	  override def inverse(v: M[A]) = app.fmap(v)(grp.inverse)
	  override def minus(l: M[A], r: M[A]) = app.<@>(grp.minus)(l)(r)
  }

  class ApplicativeRing[A, M[_]](implicit app: Applicative[M], ring: Ring[A]) extends ApplicativeGroup[A, M]
	  with Ring[M[A]] {
	  lazy val identity = app.pure(ring.identity)
	  def multiply(l: M[A], r: M[A]) = app.<@>(ring.multiply)(l)(r)
  }

  class ApplicativeField[A, M[_]](implicit app: Applicative[M], fld: Field[A]) extends ApplicativeRing[A, M]
	  with Field[M[A]] {
	  override def inverse(v: M[A]) = app.fmap(v)(fld.inverse)
	  override def divide(l: M[A], r: M[A]) = app.<@>(fld.divide)(l)(r)
  }



