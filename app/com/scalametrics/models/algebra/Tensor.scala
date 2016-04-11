package com.scalametrics.models.algebra

sealed trait Tensor[A] {
	def product(t: A): A
}

object Tensor {
	def apply[A: Tensor]: Tensor[A] = implicitly
}
