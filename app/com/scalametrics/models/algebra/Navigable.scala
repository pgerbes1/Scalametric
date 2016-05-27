package com.scalametrics.models.algebra

//Navigable...Traversable...get it?
trait Navigable[M[_]] {
	def navigate[F[_] : Applicative, A , B](a: M[A])(f: A => F[B]): F[M[B]]

}
