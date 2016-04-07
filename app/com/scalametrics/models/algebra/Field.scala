package com.scalametrics.models.algebra

trait Field[A] extends Ring[A] {
	def divide(s1: A, s2: A): A
}
object Field {
	def divide[A : Field](s1: A, s2: A): A = implicitly[Field[A]].divide(s1, s2)
	def empty[A: Field]: A = implicitly[Field[A]].empty
	def add[A : Field](s1: A, s2: A): A = implicitly[Field[A]].add(s1, s2)
	def inverse[A](v: A)(implicit fld: Field[A]) = fld.inverse(v)
	def minus[A](s1: A, s2: A)(implicit fld: Field[A]) = fld.add(s1, fld.inverse(s2))
	def identity[A : Field]: A = implicitly[Field[A]].identity
	def multiply[A : Field](s1: A, s2: A): A = implicitly[Field[A]].multiply(s1, s2)

	def derive[A](z: => A)(i: => A)(adder: (A, A) => A)
	             (multiplier: (A, A) => A)(divider: (A, A) => A)(inverter: A => A): Field[A] = new Field[A] {
		lazy val empty = z
		lazy val identity = i
		def inverse(v: A): A = inverter(v)
		def multiply(m1: A, m2: A): A = multiplier(m1, m2)
		def add(s1: A, s2: A): A = adder(s1, s2)
		def divide(s1: A, s2: A): A = divider(s1, s2)
	}
	//REMINDER: No implicit for Int. (x: Int)  => x / 2 does not always return Int
	implicit val doubleField: Field[Double] = new Field[Double] {
		def empty = 0.0
		def identity = 1.0
		def inverse(v: Double) = -v
		def divide(s1: Double, s2: Double) = if (s2 == empty) empty else s1 / s2
		def add(s1: Double, s2: Double) = s1 + s2
		def multiply(s1: Double, s2: Double) =  s1 * s2
	}
	implicit val floatField: Field[Float] = new Field[Float] {
		def empty = 0.0.toFloat
		def identity = 1.0.toFloat
	  def inverse(v: Float) = -v
		def divide(s1: Float, s2: Float) =  if (s2 == empty) empty else s1 / s2
		def add(s1: Float, s2: Float) = s1 + s2
		def multiply(s1: Float, s2: Float) =  s1 * s2
	}
	implicit class FieldOps[A : Field](f: A) {
		def divide(f2: A) = implicitly[Field[A]].divide(f, f2)
	}
}
