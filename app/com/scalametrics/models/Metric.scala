package com.scalametrics.models

object Metric {
  def apply[A: Metric](i: A, j: A): Double = implicitly[Metric[A]].apply(i, j)
  def norm[A: Metric: Monoid](i: A) = apply(i, implicitly[Monoid[A]].empty)
  def from[A](f: (A, A) => Double) = new Metric[A] {
    def apply(i: A, j: A) = f(i, j)
  }
  def normLN[A: Metric : Monoid](l: Double): Metric[Iterable[A]] =
    Metric.from {
      (i: Iterable[A], j: Iterable[A]) =>
        val d = i.zip(j).map {
          case (a, b) =>
            math.pow(implicitly[Metric[A]].apply(a, b), l)
        }.sum
        math.sqrt(d)
    }
  implicit val doubleMetric = Metric.from((a: Double, b: Double) => math.abs(a - b))
  implicit val intMetric = Metric.from((a: Int, b: Int) => math.abs((a - b).toDouble))
  implicit val longMetric = Metric.from((a: Long, b: Long) => math.abs((a - b).toDouble))
  implicit val floatMetric = Metric.from((a: Float, b: Float) => math.abs(a.toDouble - b.toDouble))
  implicit val shortMetric = Metric.from((a: Short, b: Short) => math.abs((a - b).toDouble))
  implicit val boolMetric = Metric.from((x: Boolean, y: Boolean) => if (x ^ y) 1.0 else 0.0)

  implicit def iterableMetric[V: Metric: Monoid] = normLN[V](2.0)
}
trait Metric[-A] {
  def apply(i: A, j: A): Double
}