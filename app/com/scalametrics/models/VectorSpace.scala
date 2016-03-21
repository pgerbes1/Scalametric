package com.scalametrics.models

import scala.languageFeature.higherKinds

trait VectorSpace[A, B[_]] {
	def scale(s: A, v: B[A]): B[A]
}
object VectorSpace {
	def scale[A, B[_]](s: A, r: B[A])(implicit vs: VectorSpace[A, B]): B[A] = vs.scale(s, r)
	def from[A, B[A]](scalarFunction: (A, B[A]) => B[A])(implicit n: Numeric[A]) = new VectorSpace[A, B] {
		def scale(s: A, v: B[A]) =  scalarFunction(s, v)
	}
}

