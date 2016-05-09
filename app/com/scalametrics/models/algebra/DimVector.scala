package com.scalametrics.models.algebra

 sealed trait DimVector[A]
 case class Vector1D[A](a: A) extends DimVector[A]
 case class Vector2D[A](a: A, b: A) extends DimVector[A]
 case class Vector3D[A](a: A, b: A, c: A) extends DimVector[A]

 object DimVector {

	 implicit def dimVecGroup[A](implicit grp: Group[A], app: Applicative[DimVector]): Group[DimVector[A]] = new Group[DimVector[A]] {
	   def empty: DimVector[A] = app.pure(grp.empty)
		 def add(v1: DimVector[A], v2: DimVector[A]): DimVector[A] = app.<@>{grp.add}(v1)(v2)
		 override def inverse(v: DimVector[A]) = app.fmap(v)(grp.inverse)
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
