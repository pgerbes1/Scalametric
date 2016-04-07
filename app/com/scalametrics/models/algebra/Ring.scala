package com.scalametrics.models.algebra

trait Ring[A] extends Group[A] {
  def identity: A
	def multiply(s1: A, s2: A): A
}
object Ring {
	def empty[A: Ring]: A = implicitly[Ring[A]].empty
	def add[A : Ring](s1: A, s2: A): A = implicitly[Ring[A]].add(s1, s2)
	def inverse[A](v: A)(implicit rng: Ring[A]) = rng.inverse(v)
	def minus[A](s1: A, s2: A)(implicit rng: Ring[A]) = rng.add(s1, rng.inverse(s2))
	def identity[A : Ring]: A = implicitly[Ring[A]].identity
	def multiply[A : Ring](s1: A, s2: A): A = implicitly[Ring[A]].multiply(s1, s2)
	

	def derive[A](e: => A)(i: => A)(adder: (A, A) => A)
	             (multiplier: (A, A) => A)
	             (inverter: A => A): Group[A] = new Group[A] {
		lazy val empty = e
		lazy val identity = i
		def multiply(m1: A, m2: A): A = multiplier(m1, m2)
		def add(s1: A, s2: A): A = adder(s1, s2)
		def inverse(v: A): A = inverter(v)
	}
	implicit val intRing = new Ring[Int] {
		def empty: Int = 0
		def identity: Int = 1
    def inverse(v: Int) = -v
		def multiply(m1: Int, m2: Int): Int = m1 * m2
		def add(s1: Int, s2: Int): Int = s1 + s2
	}
	implicit val doubleRing = new Ring[Double] {
		def empty: Double = 0.0
		def identity: Double = 1.0
		def inverse(v: Double) = -v
		def multiply(m1: Double, m2: Double): Double = m1 * m2
		def add(s1: Double, s2: Double): Double = s1 + s2
	}
	implicit val floatRing = new Ring[Float] {
		def empty = 0.0.toFloat
		def identity: Float = 1.0.toFloat
		def inverse(v: Float) = -v
		def multiply(m1: Float, m2: Float): Float = m1 * m2
		def add(s1: Float, s2: Float) = s1 + s2
	}
  implicit class RingOps[A : Ring](r: A) {
	  def empty = implicitly[Ring[A]].empty
	  def identity = implicitly[Ring[A]].identity
	  def inverse = implicitly[Ring[A]].inverse(r)
	  def add(r2: A) = implicitly[Ring[A]].add(r, r2)
	  def minus(r2: A)(implicit rng: Ring[A]) = rng.add(r, rng.inverse(r2))
	  def multiply(r2: A) = implicitly[Ring[A]].multiply(r, r2)
  }
}
