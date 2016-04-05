package com.scalametrics.models

trait Functor[M[_]] {
	def fmap[A, B](m: M[A])(fn: (A) => B): M[B]
}
object Functor {
	def apply[M[_] : Functor]: Functor[M] = implicitly
	def fmap[M[_] : Functor, A, B](m: M[A])(fn: A => B) = implicitly[Functor[M]].fmap(m)(fn)
}