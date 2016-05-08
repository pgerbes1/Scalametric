package  com.scalametrics.models.algebra

  trait Monoid[A] extends Semigroup[A] {
	  def empty: A
  }
  object Monoid {
	  def empty[A: Monoid]: A = implicitly[Monoid[A]].empty

	  def add[A : Monoid](s1: A, s2: A): A = implicitly[Monoid[A]].add(s1, s2)

	  def derive[A](e: => A)(associativeOp: (A, A) => A): Monoid[A] = new Monoid[A] {
		  lazy val empty = e
		  def add(s1: A, s2: A): A = associativeOp(s1, s2)
	  }
	  implicit val intMonoid = new Monoid[Int] {
		  def empty = 0
		  def add(s1: Int, s2: Int) = s1 + s2
	  }
	  implicit val doubleMonoid = new Monoid[Double] {
		  def empty = 0.0
		  def add(s1: Double, s2: Double) = s1 + s2
	  }
	  implicit val floatMonoid = new Monoid[Float] {
		  def empty = 0.0.toFloat
		  def add(s1: Float, s2: Float) = s1 + s2
	  }
	  implicit def indexedSeqMonoid[A](implicit m: Monoid[A]) = new Monoid[IndexedSeq[A]] {
		  def empty: IndexedSeq[A] = IndexedSeq()
		  def add(l1: IndexedSeq[A], l2: IndexedSeq[A]): IndexedSeq[A] = IndexedSeq((l1++l2).fold(m.empty)(m.add(_, _)))
	  }
	  implicit class MonoidOps[A : Monoid](s1: A) {
		  def empty = implicitly[Monoid[A]].empty
		  def add(s2: A) = implicitly[Monoid[A]].add(s1, s2)
	  }
  }

