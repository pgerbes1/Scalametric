package  com.scalametrics.models.algebra

trait Monad[M[_]] extends Applicative[M] {
	override def pure[A](v: A): M[A]
	def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
	override def fmap[A, B](m: M[A])(f: A => B): M[B] = flatMap(m)(a => pure(f(a)))
	override def splat[A, B](m: M[A])(f: M[A => B]) = flatMap(m)(a => fmap(f)(b => b(a)))
	def flatten[A](m: M[M[A]]): M[A] = flatMap(m)(m => m)
	def compose[A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = a => flatMap(f1(a))(f2)
	def join[A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C): M[C] = flatMap(m1)(a => fmap(m2)(b => f(a, b)))
}
object Monad {
	def apply[M[_] : Monad]: Monad[M] = implicitly
	def pure[M[_] : Monad, A](v: A): M[A] = implicitly[Monad[M]].pure(v)
	def fmap[M[_] : Monad, A, B](m: M[A])(f: A => B): M[B] = implicitly[Monad[M]].fmap(m)(f)
	def flatMap[M[_] : Monad, A, B](m: M[A])(f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(m)(f)
	def flatten[M[_] : Monad, A](m: M[M[A]]): M[A] = implicitly[Monad[M]].flatten(m)
	def compose[M[_]: Monad, A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = implicitly[Monad[M]].compose(f1)(f2)
	def join[M[_]: Monad, A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C): M[C]  = implicitly[Monad[M]].join(m1)(m2)(f)

	implicit val listMonad: Monad[List] = new Monad[List] {
		def pure[A](v: A): List[A] = List(v)
		def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l.flatMap(f)
	}
	implicit val option: Monad[Option] = new Monad[Option] {
		def pure[A](v: A): Option[A] = Option(v)
		def flatMap[A, B](o: Option[A])(f: A => Option[B]): Option[B] = o.flatMap(f)
	}
  implicit class MonadOps[A, M[_] : Monad](m: M[A]) {
	  def pure(v: A): M[A] = implicitly[Monad[M]].pure(v)
	  def fmap[B](f: A => B): M[B] = implicitly[Monad[M]].fmap(m)(f)
	  def splat[B](f: M[A => B]): M[B] = implicitly[Monad[M]].splat(m)(f)
		def flatMap[B](f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(m)(f)
		def flatten[A](m: M[M[A]]): M[A] = implicitly[Monad[M]].flatten(m)
	  def join[B, C](m2: M[B])(f: (A, B) => C): M[C] = implicitly[Monad[M]].join(m)(m2)(f)
	}
}