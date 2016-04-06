package  com.scalametrics.models.algebra

trait Monad[M[_]] extends Applicative[M] {
  def unit[A]: M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
	override def fmap[A, B](m: M[A])(fn: A => B): M[B] = flatMap(m)((a: A) => apply(fn(a)))
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
	def map[M[_] : Monad, A, B](m: M[A])(f: A => B) = implicitly[Monad[M]].fmap(m)(f)
}