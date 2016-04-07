package  com.scalametrics.models.algebra

//Provide implementation for flatMap and Monad laws take care of the rest
// Using
trait Monad[M[_]] extends Applicative[M] {
  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
	def flatten[A](m: M[M[A]]): M[A] = flatMap(m)(m => m)
	def compose[A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = a => flatMap(f1(a))(f2)
	def join[A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C): M[C] = flatMap(m1){
		a => fmap(m2) {
			b => f(a, b)
		}
	}
}
object Monad {
	def apply[M[_] : Monad]: Monad[M] = implicitly
	def flatMap[M[_] : Monad, A, B](m: M[A])(f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(m)(f)
	def flatten[M[_] : Monad, A](m: M[M[A]]): M[A] = implicitly[Monad[M]].flatten(m)
	def compose[M[_]: Monad, A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = implicitly[Monad[M]].compose(f1)(f2)
	def join[M[_]: Monad, A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C): M[C]  = implicitly[Monad[M]].join(m1)(m2)(f)

  implicit class MonadOps[A, M[_] : Monad](m: M[A]) {
		def flatMap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(m)(f)
		def flatten[A](m: M[M[A]]): M[A] = implicitly[Monad[M]].flatten(m)
	  def compose[B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = implicitly[Monad[M]].compose(f1)(f2)
	  def join[B, C](m2: M[B])(f: (A, B) => C): M[C] = implicitly[Monad[M]].join(m)(m2)(f)
	}
}