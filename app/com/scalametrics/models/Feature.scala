package com.scalametrics.models

//TODO Finish once we have Vector Field and Coordinate Frames
trait Feature[A] {
  def apply(x: A): Vector[A]
}
object Feature {
  implicit object IntFeature extends Feature[Int] {
    def apply(x: Int): Vector[Int] ={
       Vector.fill(x)(0)
    }
  }
  implicit object StringFeature extends Feature[String] {
    def apply(x: String): Vector[String] = {
        val l = x.length
        Vector.fill(l)("Ha")
    }
  }
}