package com.scalametrics.models.algebra

trait Tensor[V] {
	def n: Int
	def m: Int
	def apply[VS : VectorSpace, F : Field]
	(vs: List[V], vds: List[V => Double]): Double
}

case class TUnit[V](v: V) extends Tensor[V] {
	val n = 1
	val m = 0
	def apply(vs: List[V], vds: List[V => Double]): Double = vds head(v)
}

case class TCoUnit[V](vd: V => Double) extends Tensor[V] {
	val n = 0
	val m = 1
	def apply(vs: List[V], vds: List[V => Double]): Double = vd(vs head)
}

case class TProduct[V](t: Tensor[V], u: Tensor[V]) extends Tensor[V] {
	val n = t.n + u.n
	val m = t.m + u.m
	def apply(vs: List[V], vds: List[V => Double]): Double = {
		val (fvs, lvs) = vs splitAt(t.m)
		val (fvds, lvds) = vds splitAt(t.n)
		t.apply(fvs, fvds) * u.apply(lvs, lvds)
	}
}