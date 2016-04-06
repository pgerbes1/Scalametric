package  com.scalametrics.models.algebra

trait Functor[M[_]] {
	def fmap[A, B](m: M[A])(fn: (A) => B): M[B]
}
object Functor {
	def apply[M[_] : Functor]: Functor[M] = implicitly
	def fmap[M[_] : Functor, A, B](m: M[A])(f: A => B) = implicitly[Functor[M]].fmap(m)(f)
	implicit def ops[A, M[_] : Functor](m: M[A]) = new FunctorOps(m)(implicitly[Functor[M]])
}
class FunctorOps[A, M[_] : Functor](m: M[A]) {
	def fmap[B](f: A => B): M[B] =  implicitly[Functor[M]].fmap(m)(f)
}
