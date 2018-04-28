package functor

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Implicits {
  implicit object OptionFunctor extends Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }
}

object FunctorTest extends App {
  import Implicits._

  def identityLaw[F[_], A](fa: F[A])(implicit func: Functor[F]): Boolean =
    func.map(fa)(identity) == fa

  def compositeLaw[F[_], A, B, C](fa: F[A], f1: A => B, f2: B => C)(implicit func: Functor[F]): Boolean =
    func.map(fa)(f2 compose f1) == func.map(func.map(fa)(f1))(f2)

  val n: Option[Int] = Some(2)
  println(s"${identityLaw(n)}")
  println(s"${compositeLaw(n, (i: Int) => i * i, (i: Int) => i.toString)}")
}

