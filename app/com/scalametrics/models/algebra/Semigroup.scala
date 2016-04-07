package com.scalametrics.models.algebra

trait Semigroup[A] {
	def add(s1: A, s2: A): A
}
object Semigroup {
 def add[A : Semigroup](s1: A, s2: A): A = implicitly[Semigroup[A]].add(s1, s2)
 def derive[A](associativeOp: (A, A) => A): Semigroup[A] = new Semigroup[A] {
	  def add(s1: A, s2: A): A = associativeOp(s1, s2)
 }
	implicit val doubleMonoid = new Semigroup[Double] {
		def add(s1: Double, s2: Double) = s1 + s2
	}
	implicit val floatMonoid = new Semigroup[Float] {
		def add(s1: Float, s2: Float) = s1 + s2
	}
	implicit class SemigroupOps[A : Semigroup](s1: A) {
		def add(s2: A) = implicitly[Semigroup[A]].add(s1, s2)
	}
}