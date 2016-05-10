package com.scalametrics.models.algebra

import scala.languageFeature.higherKinds

  trait VectorSpace[A, M[_]] {
	  implicit def field: Field[A]
	  implicit def group: Group[M[A]]
	  def scale(s: A, v: M[A]): M[A]
  }
  object VectorSpace {
	  def scale[A, M[_]](s: A, r: M[A])(implicit vs: VectorSpace[A, M]): M[A] = vs.scale(s, r)

	  def from[A, M[_]](f: (A, M[A]) => M[A])(implicit fld: Field[A], grp: Group[M[A]]) = new VectorSpace[A, M] {

		  def field = fld

		  def group = grp

		  def scale(s: A, v: M[A]) = if (field.empty != s) f(s, v) else grp.empty
	  }
	  implicit def IndexedSeqSpace[A: Field] = from[A, IndexedSeq] {
		  (s, seq) => seq.map {
			  scale => Field.multiply(s, scale)
		  }
	  }
	  implicit class VectorSpaceOps[A, M[_]](r: M[A])(implicit vs: VectorSpace[A, M]) {
		  def scale(s: A) = vs.scale(s, r)
	  }
  }
/*

  trait NormedVectorSpace[A, M[_]] extends  VectorSpace[A, M] with Metric[A] {
	  def normalize(v: M[A]): M[A]
  }

  object NormedVectorSpace {
	  def apply[M[_] : NormedVectorSpace, A]: NormedVectorSpace[A, M] = implicitly
	  def normalize[M[_], A](v: M[A])(implicit nvs: NormedVectorSpace[A, M]): M[A] = nvs.normalize(v)
  }

  trait InnerProductSpace[A, M[_]] extends VectorSpace[A , M] {
	  def dot(v: M[A], w: M[A]): A
  }

  object InnerProductSpace {
	  def apply[M[_] : InnerProductSpace, A]: InnerProductSpace[A, M] = implicitly
	  def dot[M[_], A](v: M[A], v2: M[A])(implicit ips: InnerProductSpace[A, M]): A = ips.dot(v, v2)
  }
*/
