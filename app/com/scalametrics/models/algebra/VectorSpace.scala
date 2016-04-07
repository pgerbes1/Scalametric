package com.scalametrics.models.algebra

import scala.languageFeature.higherKinds

trait VectorSpace[A, M[_]] {
	def scale(s: A, v: M[A]): M[A]
}
object VectorSpace {
	def scale[A, M[_]](s: A, r: M[A])(implicit vs: VectorSpace[A, M]): M[A] = vs.scale(s, r)
	def from[A, M[_]](f: (A, M[A]) => M[A]) = new VectorSpace[A, M] {
		def scale(s: A, v: M[A]) = f(s, v)
	}
	implicit val listDouMleVectorSpace = new VectorSpace[Double, List] {
		 def scale(s: Double, r: List[Double]) = r.map(v => s * v)
	}
	implicit val listFloatVectorSpace = new VectorSpace[Float, List] {
		def scale(s: Float, r: List[Float]) = r.map(v => s * v)
	}
	implicit class VectorSpaceOps[A, M[_]](r: M[A])(implicit vs: VectorSpace[A, M]) {
		def scale(s: A) = vs.scale(s, r)
	}
}

