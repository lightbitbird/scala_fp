package monoid

object FreeMonad extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  sealed trait Free[F[_], A] {
    def map[B](f: A => B)(implicit F: Functor[F]): Free[F, B] = flatMap(a => Pure(f(a)))

    def flatMap[B](f: A => Free[F, B])(implicit F: Functor[F]): Free[F, B] =
      this match {
        case Pure(a) => f(a)
        case Impure(ff) => Impure(F.map(ff)(_.flatMap(f)))
      }
  }

  case class Pure[F[_], A](a: A) extends Free[F, A]

  case class Impure[F[_], A](ff: F[Free[F, A]]) extends Free[F, A]


  type Pair[A] = (A, A)

  implicit val PairFunctor: Functor[Pair] =
    new Functor[Pair] {
      def map[A, B](fa: Pair[A])(f: A => B): Pair[B] =
        fa match {
          case (x, y) => (f(x), f(y))
        }
    }

  type Tree[A] = Free[Pair, A]

  def leaf[A](a: A): Tree[A] = Pure(a)

  def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]])

  val r = for {
    x <- node(leaf(0), node(leaf(1), leaf(2)))
    y <- node(leaf(x), leaf(x))
  } yield y + 1

  println(r.toString)

}
