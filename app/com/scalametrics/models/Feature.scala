package com.scalametrics.models

//TODO Finish once we have Vector Field and Coordinate Frames
trait Feature[A] {
  def apply(x: A): A
}
object Feature {
  implicit object IntFeature extends Feature[Int] {
    def apply(x: Int): Vector[Double] = {
       Vector.fill(x)(0.0)
    }
  }
  implicit object StringFeature extends Feature[String] {
    def apply(x: String): Vector[Double] = {
        val l = x.length
        Vector.fill(l)(0.0)
    }
  }
}