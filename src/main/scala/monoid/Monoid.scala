package monoid

trait Monoid[F] {
  def append(a: F, b: F): F
  def zero: F
}

object MonoidImplicit {
  implicit object OptionIntMonoid extends Monoid[Option[Int]] {
    def append(a: Option[Int], b: Option[Int]): Option[Int] = (a, b) match {
      case (None, None) => None
      case (Some(v), None) => Some(v)
      case (None, Some(v)) => Some(v)
      case (Some(v1), Some(v2)) => Some(v1 + v2)
    }
    def zero: Option[Int] = None
  }
}

object MonoidTest extends App {
  import MonoidImplicit._

  def leftIdentityLaw[F](a: F)(implicit F: Monoid[F]): Boolean = a == F.append(F.zero, a)
  def rightIdentityLaw[F](a: F)(implicit F: Monoid[F]): Boolean = a == F.append(a, F.zero)
  def associativeLaw[F](a: F, b: F, c: F)(implicit F: Monoid[F]): Boolean = {
    F.append(F.append(a, b), c) == F.append(a, F.append(b, c))
  }

  val n: Option[Int] = Some(3)
  val m: Option[Int] = Some(2)
  val o: Option[Int] = Some(4)
  println(s"${leftIdentityLaw(n)}")
  println(s"${rightIdentityLaw(n)}")
  println(s"${associativeLaw(n, m, o)}")
}