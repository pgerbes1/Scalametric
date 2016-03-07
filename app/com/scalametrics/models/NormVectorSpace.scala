package com.scalametrics.models

import scala.languageFeature.implicitConversions
//TODO
trait NormVectorSpace[A, B] extends Any with VectorSpace[A, B] with Metric[A] {
	def norm(v: A): B
}
object NormVectorSpace {
	def distance[A](v: A) = implicitly[Metric[A]].apply(v, v)
}

