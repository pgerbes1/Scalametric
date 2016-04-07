package  com.scalametrics.models.algebra

trait Monoid[A] extends Semigroup[A] {
	def empty: A
}
object Monoid {
	def empty[A: Monoid](m: A): A = implicitly[Monoid[A]].empty
	def operator[A : Monoid](s1: A, s2: A): A = implicitly[Monoid[A]].operator(s1, s2)
	def derive[A](z: => A)(associativeOp: (A, A) => A): Monoid[A] = new Monoid[A] {
		lazy val empty = z
		def operator(s1: A, s2: A): A = associativeOp(s1, s2)
	}
	implicit val doubleMonoid = new Monoid[Double] {
		def empty = 0.0
		def operator(s1: Double, s2: Double) = s1 * s2
	}
	implicit val floatMonoid = new Monoid[Float] {
		def empty = 0.0.toFloat
		def operator(s1: Float, s2: Float) = s1 * s2
	}
	implicit class MonoidOps[A : Monoid](m: A) {
		def empty = implicitly[Monoid[A]].empty
		def operator(s1: A, s2: A) = implicitly[Monoid[A]].operator(s1, s2)
	}
}

