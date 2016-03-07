package com.scalametrics.models

import cats.Monoid

trait Field[A] extends Monoid[A] {
  def one: A
  def negate(l: A): A
  def combine(l: A, r: A): A
  def minus(l: A, r: A): A
  def times(l: A, r: A): A
  def div(l: A, r: A): A = times(l, negate(r))
}
object Field {
  def one[A](implicit fld: Field[A]) = fld.one
  def empty[A](l: A)(implicit ev: Monoid[A]) = ev.isEmpty(l)
  def negate[A](l: A)(implicit fld: Field[A]) = div(one,fld)
  def combine[A](l: A, r: A)(implicit monoid: Monoid[A]): A = monoid.combine(l, r)
  def minus[A](l: A, r: A)(implicit fld: Field[A]) = fld.minus(l, r)
  def times[A](l: A, r: A)(implicit fld: Field[A]) = fld.times(l, r)
  def div[A](l: A, r: A)(implicit fld: Field[A]) = fld.div(l, r)

  implicit val floatField: Field[Float] = FloatField
  implicit val doubleField: Field[Double] = DoubleField
  implicit val boolField: Field[Boolean] = BooleanField
}
object FloatField extends Field[Float] {
  override def one = 1.0f
  override def empty = 0.0f
  override def negate(v: Float) = -v
  override def combine(l: Float, r: Float) = l + r
  override def minus(l: Float, r: Float) = l - r
  override def times(l: Float, r: Float) = l * r
  override def div(l: Float, r: Float) = l / r
}
object DoubleField extends Field[Double] {
  override def one = 1.0
  override def empty = 0.0
  override def negate(v: Double) = -v
  override def combine(l: Double, r: Double) = l + r
  override def minus(l: Double, r: Double) = l - r
  override def times(l: Double, r: Double) = l * r
  override def div(l: Double, r: Double) = 1 / r
}
object BooleanField extends Field[Boolean] {
  override def one = true
  override def empty = false
  override def negate(v: Boolean) = true
  override def combine(l: Boolean, r: Boolean) = l ^ r
  override def minus(l: Boolean, r: Boolean) = l ^ r
  override def times(l: Boolean, r: Boolean) = l && r
  override def div(l: Boolean, r:Boolean) = l
}
