package com.scalametrics.models

import scala.languageFeature.higherKinds

trait VectorSpace[A, B] {
	implicit def field: Field[A]
	def scalar(s: A, v: B): B
}
object VectorSpace {
	def scalar[A, B](s: A, r: B)(implicit vs: VectorSpace[A, B]): B = vs.scalar(s, r)
	def apply[A, B](scalarFunction: (A, B) => B)(implicit fld: Field[A]) = new VectorSpace[A, B] {
		def field = fld
		def scalar(s: A, v: B) = if (!field.isEmpty(s)) scalarFunction(s, v) else v
	}
	implicit def indexedSpace[A: Field] = apply[A, IndexedSeq[A]] {
		(s, seq) => seq.map(Field.times(s, _))
	}
}