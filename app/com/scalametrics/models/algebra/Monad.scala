package  com.scalametrics.models.algebra

trait Monad[M[_]] extends Applicative[M] {
  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
	def flatten[A](m: M[M[A]]): M[A] = flatMap(m)(m => m)
	def compose[A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = a => flatMap(f1(a))(f2)
	def doubleMap[A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C): M[C] = flatMap(m1){
		a => fmap(m2) {
			b => f(a, b)
		}
	}
	override def fmap[A, B](m: M[A])(f: A => B): M[B] = flatMap(m){ a =>
		pure(f(a))
	}
}
object Monad {
	def apply[M[_] : Monad : Functor]: Monad[M] = implicitly
	def flatMap[M[_] : Monad, A, B](m: M[A])(f: A => M[B]) = implicitly[Monad[M]].flatMap(m)(f)
	def flatten[M[_] : Monad, A](m: M[M[A]]) = implicitly[Monad[M]].flatten(m)
	def compose[M[_]: Monad, A, B, C](f1: A => M[B])(f2: B => M[C]) = implicitly[Monad[M]].compose(f1)(f2)
	def doubleMap[M[_]: Monad, A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C)  = implicitly[Monad[M]].doubleMap(m1)(m2)(f)
  def fmap[M[_] : Monad: Functor, A, B](m: M[A])(f: A => B): M[B] = implicitly[Monad[M]].fmap(m)(f)


  implicit class MonadOps[A, M[_] : Monad](m: M[A]) {
		def flatMap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(m)(f)
		def flatten[A](m: M[M[A]]): M[A] = implicitly[Monad[M]].flatten(m)
	  def compose[B, C](f1: A => M[B])(f2: B => M[C]) = implicitly[Monad[M]].compose(f1)(f2)
	  def fmap[B](f: A => B) = implicitly[Monad[M]].fmap(m)(f)
	  def doubleMap[B, C](m2: M[B])(f: (A, B) => C) = implicitly[Monad[M]].doubleMap(m)(m2)(f)
	}
	implicit class PureOp[A](v: A) {
		def pure[M[_] : Monad: Applicative] = implicitly[Applicative[M]].pure(v)
	}
}