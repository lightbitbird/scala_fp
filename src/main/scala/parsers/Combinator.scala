package parsers

object ParserMain extends App {
  println(BooleanParser("true included"))
  println(BooleanParser("false included"))
  println(BooleanParser("noboolean"))
  println(PostalCodeParser("151-0093"))
  println(PostalCodeParser("868-0110"))
  println(PostalCodeParser("hoge"))
}

abstract class AbstractCombinator {

  sealed trait ParseResult[+T]

  case class Success[+T](value: T, next: String) extends ParseResult[T]

  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def string(literal: String): Parser[String] = input => {
    if (input.startsWith(literal)) {
      Success(literal, input.substring(literal.length))
    } else Failure
  }

  def s(literal: String): Parser[String] = string(literal)

  def oneOf(chars: Seq[Char]): Parser[String] = input => {
    if (input.length != 0 && chars.contains(input.head)) {
      Success(input.head.toString, input.tail)
    } else
      Failure
  }

  def select[T, U >: T](one: => Parser[T], two: => Parser[U]): Parser[U] = input => {
    one(input) match {
      case success@Success(_, _) => success
      case Failure => two(input)
    }
  }

  def combine[T, U](one: Parser[T], two: Parser[U]): Parser[(T, U)] = input => {
    one(input) match {
      case Success(value1, next1) =>
        two(next1) match {
          case Success(value2, next2) =>
            Success((value1, value2), next2)
          case Failure => Failure
        }
      case Failure => Failure
    }
  }

  def map[T, U](parser: Parser[T], function: T => U): Parser[U] = input => {
    parser(input) match {
      case Success(value, next) => Success(function(value), next)
      case Failure => Failure
    }
  }
}

