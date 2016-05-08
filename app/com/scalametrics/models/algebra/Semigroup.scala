package com.scalametrics.models.algebra

  trait Semigroup[A] {
	  def add(s1: A, s2: A): A
  }
  object Semigroup {
	  def add[A : Semigroup](s1: A, s2: A): A = implicitly[Semigroup[A]].add(s1, s2)
	  def derive[A](associativeOp: (A, A) => A): Semigroup[A] = new Semigroup[A] {
		  def add(s1: A, s2: A): A = associativeOp(s1, s2)
	  }
	  implicit val intSemigroup = new Semigroup[Int] {
		  def add(s1: Int, s2: Int) = s1 + s2
	  }
	  implicit val doubleSemigroup = new Semigroup[Double] {
		  def add(s1: Double, s2: Double) = s1 + s2
	  }
	  implicit val floatSemigroup = new Semigroup[Float] {
		  def add(s1: Float, s2: Float) = s1 + s2
	  }
	  implicit def indexSeqSemigroup[A](implicit s: Semigroup[A]) = new Semigroup[IndexedSeq[A]] {
		  def add(l1: IndexedSeq[A], l2: IndexedSeq[A]): IndexedSeq[A] = l1.zip(l2).map{case(a, b) => s.add(a, b)}
	  }
	  implicit class SemigroupOps[A : Semigroup](s1: A) {
		  def add(s2: A) = implicitly[Semigroup[A]].add(s1, s2)
	  }
  }