package  com.scalametrics.models.algebra

import scala.languageFeature.higherKinds

  trait Functor[M[_]] {
	  def fmap[A, B](m: M[A])(f: A => B): M[B]
  }

  object Functor {
	  def apply[M[_] : Functor]: Functor[M] = implicitly
	  def fmap[M[_] : Functor, A, B](m: M[A])(f: A => B): M[B] = implicitly[Functor[M]].fmap(m)(f)

	  implicit val listFunctor: Functor[List] = new Functor[List] {
		  def fmap[A, B](l: List[A])(f: A => B): List[B] = {
			  l.map(x => f(x))
		  }
	  }
	  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
		  def fmap[A, B](o: Option[A])(f: A => B): Option[B] = o match {
			  case Some(_) => Option(f(o.get))
			  case None => None
		  }
	  }
	  implicit class FunctorOps[A, M[_] : Functor](m: M[A]) {
		  def fmap[B](f: A => B): M[B] = implicitly[Functor[M]].fmap(m)(f)
	  }
  }