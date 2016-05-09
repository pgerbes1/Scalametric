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
	  implicit def OptionMonoid[A](implicit semi: Semigroup[A]) = new Monoid[Option[A]] {
		  lazy val empty = None
		  def add(o1: Option[A], o2: Option[A]): Option[A] = {
			  if (o1.isEmpty) {
				  o2
			  } else if (o2.isEmpty) {
				  o1
			  } else {
				  Some(semi.add(o2.get, o1.get))
			  }
		  }
	  }
	  implicit def ListMonoid[A] = new  Monoid[List[A]] {
		  override def empty = List[A]()
		  override def add(l1: List[A], l2: List[A]) = l1 ++ l2
	  }
	  implicit def SeqMonoid[A] = new Monoid[Seq[A]] {
		  override def empty = Seq[A]()
		  override def add(s1: Seq[A], s2: Seq[A]) = s1 ++ s2
	  }
	  
	  implicit def SetMonoid[A] = new Monoid[Set[A]] {
		  override def empty = Set[A]()
		  override def add(s1: Set[A], s2: Set[A]) = s1 ++ s2
	  }
	  implicit def Function1Monoid[A] = new Monoid[Function1[A, A]] {
		  override def empty = identity[A]
		  override def add(f1: Function1[A, A], f2: Function1[A, A]) = {
			  (t: A) => f2(f1(t))
		  }
	  }
	  implicit class MonoidOps[A : Monoid](s1: A) {
		  def empty = implicitly[Monoid[A]].empty
		  def add(s2: A) = implicitly[Monoid[A]].add(s1, s2)
	  }
  }

