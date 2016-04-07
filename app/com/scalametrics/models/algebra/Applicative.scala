package  com.scalametrics.models.algebra

trait Applicative[M[_]] extends Functor[M] {
	def pure[A](v: A): M[A]
	def splat[A, B](a: M[A])(f: M[A => B]): M[B]
}
object Applicative {
	def apply[M[_] : Applicative]: Applicative[M] = implicitly
	def pure[A, M[_] : Applicative](v: A) = implicitly[Applicative[M]].pure(v)
	def splat[M[_] : Applicative, A, B](a: M[A])(f: M[A => B]) = implicitly[Applicative[M]].splat(a)(f)

	implicit val ListApplicative: Applicative[List] = new Applicative[List] {
		def pure[A](v: A): List[A] = List(v)
		def fmap[A, B](l: List[A])(f: A => B): List[B] = {
			l.map(f(_))
		}
		def splat[A, B](l: List[A])(f: List[A => B]): List[B] = {
			for {
				i <- l
				j <- f
			} yield j(i)
		}
	}
	implicit val OptionApplicative = new Applicative[Option] {
		def pure[A](v: A): Option[A] = v match {
			case Some(_) => Option(v)
			case None => None
		}
		def fmap[A, B](o: Option[A])(f: A => B): Option[B] = o match {
			case Some(_) => Option(f(o.get))
			case None => None
		}
		def splat[A, B](o: Option[A])(f: Option[A => B]): Option[B] = o match {
			case Some(_) => Option(f.get(o.get))
			case None => None
		}
	}
	implicit class ApplicativeOps[A, M[_] : Applicative](a: M[A]) {
		def splat[B](f: M[A => B]) = implicitly[Applicative[M]].splat(a)(f)
	}
	implicit class PureOp[A](v: A) {
		def pure[M[_] : Applicative] = implicitly[Applicative[M]].pure(v)
	}
}
