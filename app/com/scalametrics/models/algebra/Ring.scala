package com.scalametrics.models.algebra

trait Ring[A] extends Group[A] {
  def identity: A
	def multiply(s1: A, s2: A): A
}
object Ring {
	def identity[A : Ring]: A = implicitly[Ring[A]].identity
	def multiply[A : Ring](s1: A, s2: A): A = implicitly[Ring[A]].multiply(s1, s2)

	def derive[A](z: => A)(i: => A)(adder: (A, A) => A)(multiplier: (A, A) => A): Group[A] = new Group[A] {
		lazy val empty = z
		lazy val identity = i
		def multiply(m1: A, m2: A): A = multiplier(m1, m2)
		def add(s1: A, s2: A): A = adder(s1, s2)
		override def inverse(v: A): A = minus(empty, v)
	}

	implicit val doubleMonoid = new Ring[Double] {
		def empty: Double = 0.0
		def identity: Double = 1.0
		override def inverse(v: Double) = -v
		def multiply(m1: Double, m2: Double): Double = m1 * m2
		def add(s1: Double, s2: Double): Double = s1 + s2
	}
	implicit val floatMonoid = new Ring[Float] {
		def empty = 0.0.toFloat
		def identity: Float = 1.0.toFloat
		override def inverse(v: Float) = -v
		def multiply(m1: Float, m2: Float): Float = m1 * m2
		def add(s1: Float, s2: Float) = s1 + s2
	}
}
