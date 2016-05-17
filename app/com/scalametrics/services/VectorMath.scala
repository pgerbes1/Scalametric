package com.scalametrics.services

import com.scalametrics.models.algebra.Functor._
import com.scalametrics.models.algebra.DimVector._
import com.scalametrics.models.algebra.Foldable._
import com.scalametrics.models.algebra.Metric._
import com.scalametrics.models.algebra.Ring._
import com.scalametrics.models.algebra.{DimVector, Monoid, Ring, Vector3D}

object VectorMath {
	def makeUnitVector(v: DimVector[Double]): DimVector[Double] = v.fmap(x => x / norm(v))

	def innerProduct[A](v: DimVector[A], w: DimVector[A])
	                   (implicit rng: Ring[DimVector[A]], mon: Monoid[A]): A = v.multiply(w).fold

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
}

