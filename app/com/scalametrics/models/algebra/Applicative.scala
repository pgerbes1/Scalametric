package  com.scalametrics.models.algebra

trait Applicative[M[_]] extends Functor[M] {
	def pure[A](v: A): M[A]
	def join[A, B](m1: M[A], m2: M[B]): M[(A, B)]
}
object Applicative {
	def apply[M[_] : Applicative]: Applicative[M] = implicitly
	def pure[A, M[_]: Applicative](v: A) = implicitly[Applicative[M]].pure(v)
	def join[M[_] : Applicative, A, B](m1: M[A], m2: M[B]): M[(A, B)] =
		implicitly[Applicative[M]].join(m1, m2)
	implicit def pureOp[A](a: A) = new PureOp(a)
	implicit def ops[A, M[_] : Applicative](a: M[A]) = new ApplicativeOps(a)(implicitly[Applicative[M]])
}
class ApplicativeOps[A, M[_] : Applicative](a: M[A]) extends FunctorOps[A, M](a) {
	def join[B](b: M[B]): M[(A, B)] = implicitly[Applicative[M]].join(a, b)
}
class PureOp[A](a: A) {
	def pure[M[_] : Applicative] = implicitly[Applicative[M]].pure(a)
}
