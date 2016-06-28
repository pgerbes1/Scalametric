package com.scalametric.models.algebra

  trait Group[A] extends Monoid[A] {
		def inverse(v: A): A = minus(empty, v)

		def minus(s1: A, s2: A): A = add(s1, inverse(s2))
	}
  object Group {
	  def apply[A : Group]: Group[A] = implicitly

	  def inverse[A](v: A)(implicit grp: Group[A]) = grp.minus(grp.empty, v)

	  def minus[A](s1: A, s2: A)(implicit grp: Group[A]) = grp.add(s1, grp.inverse(s2))

	  def empty[A: Group]: A = implicitly[Group[A]].empty

	  def add[A : Group](s1: A, s2: A): A = implicitly[Group[A]].add(s1, s2)

	  implicit val intGroup = new Group[Int] {
		  def empty = 0
		  override def inverse(v: Int) = -v
		  def add(s1: Int, s2: Int) = s1 + s2
	  }
	  implicit val doubleGroup = new Group[Double] {
		  def empty = 0.0
		  override def inverse(v: Double) = -v
		  def add(s1: Double, s2: Double) = s1 + s2
	  }
	  implicit val floatGroup = new Group[Float] {
		  def empty = 0.0.toFloat
		  override def inverse(v: Float) = -v
		  def add(s1: Float, s2: Float) = s1 + s2
	  }
	  implicit def indexedSeqGroup[A](implicit m: Group[A]) = new Group[IndexedSeq[A]] {
		  def empty: IndexedSeq[A] = IndexedSeq()
		  override def inverse(l: IndexedSeq[A]): IndexedSeq[A] = l.map(x => x.inverse)
		  def add(l1: IndexedSeq[A], l2: IndexedSeq[A]): IndexedSeq[A] = IndexedSeq((l1++l2).fold(m.empty)(m.add(_, _)))
	  }
	  implicit def OptionGroup[A](implicit group: Group[A]) = new Group[Option[A]] {
		  override def empty = None
		  override def inverse(opt: Option[A]) =
			  opt.map{ v => group.inverse(v) }
		  def add(o1: Option[A], o2: Option[A]): Option[A] = {
			  if (o1.isEmpty) {
				  o2
			  } else if (o2.isEmpty) {
				  o1
			  } else {
				  Some(group.add(o2.get, o1.get))
			  }
		  }
	  }
	  implicit class GroupOps[A : Group](g: A) {
		  def empty: A = implicitly[Group[A]].empty
		  def add(g2: A): A = implicitly[Group[A]].add(g, g2)
		  def inverse: A = implicitly[Group[A]].add(implicitly[Group[A]].empty, g)
		  def minus(g2: A): A = implicitly[Group[A]].add(g, implicitly[Group[A]].inverse(g2))
	  }
  }
