package com.scalametrics.models

import scala.languageFeature.higherKinds

trait VectorSpace[A, B[_]] {
	implicit def field: Field[B[A]]
	def scalar(s: A, v: B[A]): B[A]
}
object VectorSpace {
	def scalar[A, B[_]](s: A, r: B[A])(implicit vs: VectorSpace[A, B]): B[A] = vs.scalar(s, r)
	def apply[A, B[_]](scalarFunction: (A, B[A]) => B[A])(implicit fld: Field[B[A]]) = new VectorSpace[A, B] {
		def field = fld
		def scalar(s: A, v: B[A]) = if (!field.isEmpty(v)) scalarFunction(s, v) else v
	}
	implicit def indexedSpace[A: Field] = apply[A, IndexedSeq[A]] {
		(s, seq) => seq.map(Field.times(s, _))
	}
}

