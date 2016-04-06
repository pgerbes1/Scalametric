package  com.scalametrics.models.algebra

trait Monad[M[_]] extends Applicative[M] {
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
	override def fmap[A, B](m: M[A])(f: A => B): M[B] = flatMap(m)((a: A) => pure(f(a)))
	override def join[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
		flatMap(ma) { (a: A) =>
			fmap(mb) { (b: B) =>
				(a, b)
			}
		}
}
object Monad {
	def apply[M[_] : Monad]: Monad[M] = implicitly
	def flatMap[M[_] : Monad, A, B](m: M[A])(f: A => M[B]) = implicitly[Monad[M]].flatMap(m)(f)
	def fmap[M[_] : Monad, A, B](m: M[A])(f: A => B) = implicitly[Monad[M]].fmap(m)(f)
	implicit def pureOp[A](a: A) = new PureOp(a)
	implicit def ops[A, M[_] : Monad](m: M[A]) = new MonadOps(m)(implicitly[Monad[M]])
}
class MonadOps[A, M[_]](m: M[A])(implicit monad: Monad[M]) extends ApplicativeOps[A, M](m) {
	def flatMap[B](f: A => M[B]): M[B] = monad.flatMap(m)(f)
}