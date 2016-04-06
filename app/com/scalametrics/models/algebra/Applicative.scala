package  models.algebra

trait Applicative[M[_]] extends Functor[M] {
	def apply[A](v: A): M[A]
	def join[A, B](m1: M[A], m2: M[B]): M[(A, B)]
}
object Applicative {
	def apply[M[_] : Applicative]: Applicative[M] = implicitly
	def join[M[_] : Applicative, A, B](m1: M[A], m2: M[B]): M[(A, B)] =
		implicitly[Applicative[M]].join(m1, m2)
}