package monoid

trait SimpleMonoid[A] {
  def append(a: A, b: A): A
  def zero: A
}

object SimpleMonoidImplicit {
  implicit object OptionIntMonoid extends SimpleMonoid[Option[Int]] {
    def append(a: Option[Int], b: Option[Int]): Option[Int] = (a, b) match {
      case (None, None) => None
      case (Some(v), None) => Some(v)
      case (None, Some(v)) => Some(v)
      case (Some(v1), Some(v2)) => Some(v1 + v2)
    }
    def zero: Option[Int] = None
  }
}

object SimpleMonoidTest extends App {
  import SimpleMonoidImplicit._

  def leftIdentityLaw[A](a: A)(implicit F: SimpleMonoid[A]): Boolean = a == F.append(F.zero, a)
  def rightIdentityLaw[A](a: A)(implicit F: SimpleMonoid[A]): Boolean = a == F.append(a, F.zero)
  def associativeLaw[A](a: A, b: A, c: A)(implicit F: SimpleMonoid[A]): Boolean = {
    F.append(F.append(a, b), c) == F.append(a, F.append(b, c))
  }

  val n: Option[Int] = Some(3)
  val m: Option[Int] = Some(2)
  val o: Option[Int] = Some(4)
  println(s"${leftIdentityLaw(n)}")
  println(s"${rightIdentityLaw(n)}")
  println(s"${associativeLaw(n, m, o)}")
}