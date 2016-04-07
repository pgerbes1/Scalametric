package com.scalametrics.models.algebra

	trait Group[A] extends Monoid[A] {
		def inverse(v: A): A = minus(empty, v)
		def minus(l: A, r: A): A = add(l, inverse(r))
	}
  object Group {
		def inverse[A : Group](v: A) = implicitly[Group[A]].inverse(v)
		def minus[A : Group](s1: A, s2: A) = implicitly[Group[A]].minus(s1, s2)

	  def derive[A](z: => A)(adder: (A, A) => A): Group[A] = new Group[A] {
		  lazy val empty = z
		  def add(s1: A, s2: A): A = adder(s1, s2)
		  override def inverse(v: A): A = minus(empty, v)
	  }

	  implicit val doubleGroup = new Group[Double] {
		  def empty = 0.0
		  override def inverse(v: Double) = -v
		  def add(s1: Double, s2: Double) = s1 * s2
	  }
	  implicit val floatMonoid = new Group[Float] {
		  override def empty = 0.0.toFloat
		  def add(s1: Float, s2: Float) = s1 * s2
	  }
}