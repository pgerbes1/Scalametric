package com.scalametrics.models.algebra

trait Semigroup[A] {
	def operator(s1: A, s2: A): A
}
object Semigroup {
 def operator[A : Semigroup](s1: A, s2: A): A = implicitly[Semigroup[A]].operator(s1, s2)
 def derive[A](associativeOp: (A, A) => A): Semigroup[A] = new Semigroup[A] {
	  def operator(s1: A, s2: A): A = associativeOp(s1, s2)
 }
}