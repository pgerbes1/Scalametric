package  com.scalametrics.models.algebra

  trait Monad[M[_]] extends Applicative[M] {
	  override def pure[A](v: A): M[A]

	  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]

	  override def fmap[A, B](m: M[A])(f: A => B): M[B] = flatMap(m)(a => pure(f(a)))

	  override def <*>[A, B](m: M[A])(f: M[A => B]) = flatMap(m)(a => fmap(f)(b => b(a)))

	  def flatten[A](m: M[M[A]]): M[A] = flatMap(m)(m => m)

	  def compose[A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = a => flatMap(f1(a))(f2)

	  override def <@>[A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C): M[C] = flatMap(m1)(a => fmap(m2)(b => f(a, b)))
  }
  object Monad {
	  def apply[M[_] : Monad]: Monad[M] = implicitly

	  def pure[M[_] : Monad, A](v: A): M[A] = implicitly[Monad[M]].pure(v)

	  def flatMap[M[_] : Monad, A, B](m: M[A])(f: A => M[B]): M[B] = implicitly[Monad[M]].flatMap(m)(f)

	  def fmap[M[_], A, B](m: M[A])(f: A => B)(implicit monad: Monad[M]): M[B] = monad.flatMap(m)(a => monad.pure(f(a)))

	  def <*>[M[_], A, B](m: M[A])(f: M[A => B])(implicit monad: Monad[M]) = monad.flatMap(m)(a => monad.fmap(f)(b => b(a)))

	  def flatten[M[_] : Monad, A](m: M[M[A]]): M[A] = implicitly[Monad[M]].flatMap(m)(m => m)

	  def compose[M[_] : Monad, A, B, C](f1: A => M[B])(f2: B => M[C]): A => M[C] = a => implicitly[Monad[M]].flatMap(f1(a))(f2)

	  def <@>[M[_], A, B, C](m1: M[A])(m2: M[B])(f: (A, B) => C)(implicit monad: Monad[M]): M[C] = {
		  monad.flatMap(m1){
			  a => monad.fmap(m2){
				  b => f(a, b)
			  }
		  }
	  }

	  implicit val listMonad: Monad[List] = new Monad[List] {
		  def pure[A](v: A): List[A] = List(v)

		  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l.flatMap(f)
	  }
	  implicit val option: Monad[Option] = new Monad[Option] {
		  def pure[A](v: A): Option[A] = Option(v)

		  def flatMap[A, B](o: Option[A])(f: A => Option[B]): Option[B] = o.flatMap(f)
	  }
    implicit class MonadOps[A, M[_]](m: M[A]) {
	    def pure(v: A)(implicit monad: Monad[M]): M[A] = monad.pure(v)

	    def flatMap[B](f: A => M[B])(implicit monad: Monad[M]): M[B] = monad.flatMap(m)(f)

	    def fmap[B](f: A => B)(implicit monad: Monad[M]): M[B] = monad.flatMap(m)(a => monad.pure(f(a)))

	    def <*>[B](f: M[A => B])(implicit monad: Monad[M]): M[B] = monad.flatMap(m)(a => monad.fmap(f)(b => b(a)))

	    def flatten(m: M[M[A]])(implicit monad: Monad[M]): M[A] = implicitly[Monad[M]].flatMap(m)(m => m)

	    def compose[B, C](f1: A => M[B])(f2: B => M[C])(implicit monad: Monad[M]): A => M[C] = a => monad.flatMap(f1(a))(f2)

	    def <@>[B, C](m2: M[B])(f: (A, B) => C)(implicit monad: Monad[M]): M[C] =  {
		    monad.flatMap(m){
			    a => monad.fmap(m2){
				    b => f(a, b)
			    }
		    }
	    }
    }
  }