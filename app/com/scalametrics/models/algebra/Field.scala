package com.scalametrics.models.algebra

trait Field[A] extends Ring[A] {
	override def inverse(v: A): A = {
		divide(identity, v)
	}
	def divide(s1: A, s2: A): A = {
		multiply(s1, inverse(s2))
	}
}
object Field {
	def divide[A : Field](s1: A, s2: A): A = implicitly[Field[A]].divide(s1, s2)

	def derive[A](z: => A)(i: => A)(adder: (A, A) => A)(multiplier: (A, A) => A): Field[A] = new Field[A] {
		lazy val empty = z
		lazy val identity = i
		def multiply(m1: A, m2: A): A = multiplier(m1, m2)
		def add(s1: A, s2: A): A = adder(s1, s2)
		override def inverse(v: A): A = minus(empty, v)
	}
	implicit val doubleField: Field[Double] = new Field[Double] {
		def empty = 0.0
		def identity = 1.0
		override def inverse(v: Double) = -v
		def add(s1: Double, s2: Double) = s1 + s2
		def multiply(s1: Double, s2: Double) =  s1 * s2
	}
	implicit val floatField: Field[Float] = new Field[Float] {
		def empty = 0.0.toFloat
		def identity = 1.0.toFloat
		override def inverse(v: Float) = -v
		def add(s1: Float, s2: Float) = s1 + s2
		def multiply(s1: Float, s2: Float) =  s1 * s2
	}
	
}
