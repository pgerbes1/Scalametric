package com.scalametrics.models.algebra

  trait Metric[-A] {
	  def distance(i: A, j: A): Double
  }
  object Metric {
	  def distance[A: Metric](i: A, j: A): Double = implicitly[Metric[A]].distance(i, j)

	  def norm[A: Metric: Monoid](i: A) = implicitly[Metric[A]].distance(i, implicitly[Monoid[A]].empty)

	  def from[A](f: (A, A) => Double) = new Metric[A] {
		def distance(i: A, j: A) = f(i, j)
	}
	  def minkowskiDistance[A : Monoid: Metric](p: Double): Metric[Iterable[A]] = Metric.from{
		  (a: Iterable[A], b: Iterable[A]) =>
			  val maxSize = scala.math.max(a.size, b.size)
			  def pad(v: Iterable[A]) = {
				  val diff = maxSize - v.size
				  if (diff > 0) {
					  v ++ Iterator.fill(diff)(Monoid.empty[A])
				  } else {
					  v
				  }
			  }
			  val outP = pad(a).view
				  .zip(pad(b))
				  .map {
				  	case (i, j) =>
					  	math.pow(implicitly[Metric[A]].distance(i, j), p)
				  }.sum
			  math.pow(outP, 1.0 / p)
	  }

	  implicit val doubleMetric = Metric.from((a: Double, b: Double) => math.abs(a - b))
	  implicit val intMetric = Metric.from((a: Int, b: Int) => math.abs((a - b).toDouble))
	  implicit val floatMetric = Metric.from((a: Float, b: Float) => math.abs(a.toDouble - b.toDouble))

	  implicit def iterableMetric[A: Metric: Monoid] = minkowskiDistance[A](2.0)

	  implicit class MetricOps[A : Metric : Monoid](i: A) {
		  def distance(j: A): Double = implicitly[Metric[A]].distance(i, j)
		  def norm = implicitly[Metric[A]].distance(i, implicitly[Monoid[A]].empty)
	  }
  }
