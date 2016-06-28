  package com.scalametric.models.algebra

  import Ring._
  import Foldable._
  import Metric._
  import Functor._

  sealed trait DimVector[+A]
  final case class Vector1D[+A](a: A) extends DimVector[A]
  final case class Vector2D[+A](a: A, b: A) extends DimVector[A]
  final case class Vector3D[+A](a: A, b: A, c: A) extends DimVector[A]

  object DimVector {

	  def apply[A](v: A*) = v.length match  {
		  case 1 => Vector1D(v(0))
		  case 2 => Vector2D(v(0), v(1))
		  case 3 => Vector3D(v(0),v(1), v(2))
		  case _ => sys.error("Can't have more than 3 dimensions")
	  }

	  def makeUnitVector(v: DimVector[Double]): DimVector[Double] = v.fmap(_ / norm(v))

	  def innerProduct[A](v: DimVector[A], w: DimVector[A])(implicit r: Ring[DimVector[A]], m: Monoid[A]): A =
		  r.multiply(v, w).fold

	  def crossProduct[A](v: Vector3D[A],
	                      w: Vector3D[A])(implicit rng: Ring[A]): Vector3D[A] = {
		  def crossHelper(m: A, n: A)(p: A, q: A): A = {
			  rng.minus(rng.multiply(m, n), rng.multiply(p, q))
		  }
		  val s1 = crossHelper(v.c, w.b)(v.b, w.c)
		  val s2 = crossHelper(v.a, w.c)(v.c, w.a)
		  val s3 = crossHelper(v.a, w.b)(v.b, w.a)
		  Vector3D(s1, s2, s3)
	  }

	  def scalarTripleProduct[A](v: Vector3D[A], w: Vector3D[A], u: Vector3D[A])
	                            (implicit rng: Ring[A]): A = innerProduct(v, crossProduct(w, u))

	  def vectorTripleProduct[A](v: Vector3D[A], w: Vector3D[A], u: Vector3D[A])
	                            (implicit rng: Ring[A]): Vector3D[A] = crossProduct(v, crossProduct(w, u))

	  implicit def dimVecSemigroup[A : Semigroup]: Semigroup[DimVector[A]] = new ApplicativeSemigroup[A, DimVector]
	  implicit def dimVecMonoid[A : Monoid]: Monoid[DimVector[A]] = new ApplicativeMonoid[A, DimVector]
	  implicit def dimVecGroup[A : Group]: Group[DimVector[A]] = new ApplicativeGroup[A, DimVector]
	  implicit def dimVecRing[A : Ring]: Ring[DimVector[A]] = new ApplicativeRing[A, DimVector]
	  implicit def dimVecField[A : Field]: Field[DimVector[A]] = new ApplicativeField[A, DimVector]

	  implicit def dimVecSpace[A: Field] = VectorSpace.from[A, DimVector]{ (s: A, vec: DimVector[A]) =>
		  Functor.fmap(vec)(Ring.multiply(s, _))
	  }

	  implicit val dimVecMetric = Metric.from { (v: DimVector[Double], w: DimVector[Double]) =>
		  val sub = Ring.minus(v, w)
		  math.sqrt(Foldable.fold(Ring.multiply(sub, sub)))
	  }

	  implicit val dimVecFoldable: Foldable[DimVector] = new Foldable[DimVector] {
		  override def foldLeft[A, B](fa: DimVector[A])(init: B)(f: (B, A) => B): B =
			  fa match {
				  case Vector1D(a) => f(init, a)
				  case Vector2D(a, b) => f(f(init, a), b)
				  case Vector3D(a, b, c) => f(f(f(init, a), b), c)
			  }

		  override def foldRight[A, B](fa: DimVector[A])(init: B)(f: (A, B) => B): B =
			  fa match {
				  case Vector1D(a) => f(a, init)
				  case Vector2D(a, b) => f(b, f(a, init))
				  case Vector3D(a, b, c) => f(c, f(b, f(a, init)))
			  }
	  }

	  implicit val dimVecApplicative: Applicative[DimVector] = new Applicative[DimVector] {
		  def pure[A](a: A): DimVector[A] = Vector3D(a, a, a)

		  def fmap[A, B](fa: DimVector[A])(f: A => B): DimVector[B] =
			  fa match {
				  case Vector1D(a) => Vector1D(f(a))
				  case Vector2D(a, b) => Vector2D(f(a), f(b))
				  case Vector3D(a, b, c) => Vector3D(f(a), f(b), f(c))
			  }

		  def <*>[A, B](fa: DimVector[A])(f: DimVector[A => B]): DimVector[B] =
			  fa match {
				  case Vector1D(a) =>
					  f match {
						  case Vector1D(g) => Vector1D(g(a))
						  case Vector2D(g, _) => Vector1D(g(a))
						  case Vector3D(g, _, _) => Vector1D(g(a))
					  }
				  case Vector2D(a, b) =>
					  f match {
						  case Vector1D(g) => Vector1D(g(a))
						  case Vector2D(g, h) => Vector2D(g(a), h(b))
						  case Vector3D(g, h, _) => Vector2D(g(a), h(b))
					  }
				  case Vector3D(a, b, c) =>
					  f match {
						  case Vector1D(g) => Vector1D(g(a))
						  case Vector2D(g, h) => Vector2D(g(a), h(b))
						  case Vector3D(g, h, i) => Vector3D(g(a), h(b), i(c))
					  }
			  }
	  }
  }