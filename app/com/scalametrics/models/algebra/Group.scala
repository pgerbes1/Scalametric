package com.scalametrics.models.algebra

	trait Group[A] extends Monoid[A] {
		def inverse(v: A): A
		def minus(s1: A, s2: A): A = add(s1, inverse(s2))
	}
  object Group {
	  def empty[A: Group]: A = implicitly[Group[A]].empty
	  def add[A : Group](s1: A, s2: A): A = implicitly[Group[A]].add(s1, s2)
		def inverse[A](v: A)(implicit grp: Group[A]) = grp.inverse(v)
		def minus[A](s1: A, s2: A)(implicit grp: Group[A]) = grp.add(s1, grp.inverse(s2))

	  def derive[A](e: => A)(adder: (A, A) => A)(inverter: A => A): Group[A] = new Group[A] {
		  lazy val empty = e
		  def add(s1: A, s2: A): A = adder(s1, s2)
		  def inverse(v: A): A = inverter(v)
	  }
	  implicit val intGroup = new Group[Int] {
		  def empty = 0
		  def inverse(v: Int) = -v
		  def add(s1: Int, s2: Int) = s1 + s2
	  }
	  implicit val doubleGroup = new Group[Double] {
		  def empty = 0.0
		  def inverse(v: Double) = -v
		  def add(s1: Double, s2: Double) = s1 + s2
	  }
	  implicit val floatGroup = new Group[Float] {
		  def empty = 0.0.toFloat
		  def inverse(v: Float) = -v
		  def add(s1: Float, s2: Float) = s1 + s2
	  }
	  implicit class GroupOps[A : Group](g: A) {
		  def empty: A = implicitly[Group[A]].empty
		  def add(g2: A): A = implicitly[Group[A]].add(g, g2)
		  def inverse: A = implicitly[Group[A]].inverse(g)
		  def minus(g2: A): A = implicitly[Group[A]].add(g, implicitly[Group[A]].inverse(g2))
	  }
}