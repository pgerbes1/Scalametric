package com.scalametrics.models.algebra

trait Field[A] extends Ring[A] {
	def divide(s1: A, s2: A): A
}
object Field {
	def apply[A : Field]: Field[A] = implicitly

	def empty[A: Field]: A = implicitly[Field[A]].empty

	def identity[A : Field]: A = implicitly[Field[A]].identity

	def divide[A : Field](s1: A, s2: A): A = implicitly[Field[A]].divide(s1, s2)

	def multiply[A : Field](s1: A, s2: A): A = implicitly[Field[A]].multiply(s1, s2)

	def add[A : Field](s1: A, s2: A): A = implicitly[Field[A]].add(s1, s2)

	def minus[A : Field](s1: A, s2: A): A = implicitly[Field[A]].minus(s1, s2)

	def inverse[A](v: A)(implicit fld: Field[A]) = fld.divide(fld.identity, v)

	implicit val doubleField: Field[Double] = new Field[Double] {
		def empty = 0.0
		def identity = 1.0
		def divide(s1: Double, s2: Double) = if (s2 == empty) empty else s1 / s2
		def multiply(s1: Double, s2: Double) =  s1 * s2
		def add(s1: Double, s2: Double) = s1 + s2
		override def minus(s1: Double, s2: Double) = s1 - s2
	}
	implicit val floatField: Field[Float] = new Field[Float] {
		def empty = 0.0.toFloat
		def identity = 1.0.toFloat
		def divide(s1: Float, s2: Float) =  if (s2 == empty) empty else s1 / s2
		def multiply(s1: Float, s2: Float) =  s1 * s2
		def add(s1: Float, s2: Float) = s1 + s2
		override def minus(s1: Float, s2: Float) = s1 - s2
	}
	implicit class FieldOps[A : Field](f: A) {
		def empty = implicitly[Field[A]].empty
		def identity = implicitly[Field[A]].identity
		def inverse = implicitly[Field[A]].divide(implicitly[Field[A]].identity, f)
		def add(f2: A) = implicitly[Field[A]].add(f, f2)
		def minus(f2: A) = implicitly[Field[A]].minus(f, f2)
		def divide(f2: A) = implicitly[Field[A]].divide(f, f2)
		def multiply(f2: A) = implicitly[Field[A]].multiply(f, f2)
	}
}
