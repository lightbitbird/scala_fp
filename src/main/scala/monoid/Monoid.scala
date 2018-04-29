package monoid

trait Monoid[A] {
  def empty: A
  def combine(a1: A, a2: A): A
}

trait OptionMonoid[A] extends Monoid[Option[A]] {
  def empty: Option[A] = None
  def combine(a1: Option[A], a2: Option[A]): Option[A] = ???
//    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
}

object MonoidTest extends App {
  println(Monoid.intOptionMonoid.combine(Some(2), Some(3)))
  println(Monoid.intOptionMonoid.combine(Some(4), None))

  val M: Monoid[Map[String, Map[String, Int]]] =
    Monoid.mapMergeMonoid(Monoid.mapMergeMonoid(Monoid.intAddition))
  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i1" -> 3))
  val m3 = M.combine(m1, m2)
  println(m3)

  val m = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
}


object Monoid {
  def stringMonoid = new Monoid[String] {
    override def empty: String = ""

    override def combine(a1: String, a2: String): String = a1 + a2
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def empty = Nil

    def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  }

  def intAddition: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }

  def intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0

    override def combine(a1: Int, a2: Int): Int = a1 * a2
  }

  def booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  def booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  def intOptionMonoid: OptionMonoid[Int] = new OptionMonoid[Int] {
//    override def combine(a1: Option[Int], a2: Option[Int]): Option[Int] = for {
//      x <- a1
//      y <- a2
//    } yield x + y

    override def combine(a1: Option[Int], a2: Option[Int]): Option[Int] = (a1, a2) match {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(y)) => Some(y)
      case (Some(x), Some(y)) => Some(x + y)
    }
  }

  def stringOptionMonoid: OptionMonoid[String] = new OptionMonoid[String] {
    override def combine(a1: Option[String], a2: Option[String]): Option[String] = (a1, a2) match {
      case (None, None) => None
      case (Some(x), None) => Some(x)
      case (None, Some(y)) => Some(y)
      case (Some(x), Some(y)) => Some(x + y)
    }
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = {
    new Monoid[Map[K, V]] {
      val empty = Map[K, V]()

      def combine(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(empty) {
          (acc, k) => acc.updated(k, V.combine(a1.getOrElse(k, V.empty), a2.getOrElse(k, V.empty)))
        }
    }
  }

  def productMonoid[A, B](M1: Monoid[A], M2: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      val empty: (A, B) = (M1.empty, M2.empty)
      def combine(a1: (A, B), a2: (A, B)): (A, B) = (M1.combine(a1._1, a2._1), M2.combine(a1._2, a2._2))
    }

  def functionMonoid[A, B](M: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      val empty: A => B = a => M.empty
      def combine(f: A => B, g: A => B): A => B = a => M.combine(f(a), g(a))
    }
}