package monad

trait Monad[F[_]] {
  def point[A](a: A): F[A]
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object MonadImplicit {
  implicit object OptionMonad extends Monad[Option] {
    override def point[A](a: A): Option[A] = Some(a)

    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }
  }
}

object MonadTest extends App {
  import MonadImplicit._

  def rightIdentityLaw[F[_], A](fa: F[A])(implicit F: Monad[F]): Boolean =
    F.bind(fa)(F.point(_)) == fa

  def leftIdentityLaw[F[_], A, B](a: A, f: A => F[B])(implicit F: Monad[F]): Boolean =
    F.bind(F.point(a))(f) == f(a)

  def associativeLaw[F[_], A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit F: Monad[F]): Boolean =
    F.bind(F.bind(fa)(f))(g) == F.bind(fa)((a: A) => F.bind(f(a))(g))

  /** Monadの条件でApplicative.apを定義可能 **/
  def ap[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Monad[F]): F[B] =
    F.bind(f)((g: A => B) => F.bind(fa)((a: A) => F.point(g(a))))

  val fa: Option[Int] = Some(3)
  val f: Int => Option[Int] = { n => Some(n + 1) }
  val g: Int => Option[Int] = { n => Some(n * n) }

  println(s"${rightIdentityLaw(fa)}")
  println(s"${leftIdentityLaw(2, f)}")
  println(s"${associativeLaw(fa, f, g)}")

}
