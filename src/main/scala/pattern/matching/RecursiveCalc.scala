package pattern.matching

sealed abstract class Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Sub(lhs: Exp, rhs: Exp) extends Exp
case class Mul(lhs: Exp, rhs: Exp) extends Exp
case class Div(lhs: Exp, rhs: Exp) extends Exp
case class Lit(value: Int) extends Exp

object RecursiveCalc extends App {
  def eval(exp: Exp): Int = exp match {
    case Add(l, r) => eval(l) + eval(r)
    case Sub(l, r) => eval(l) - eval(r)
    case Mul(l, r) => eval(l) * eval(r)
    case Div(l, r) => eval(l) / eval(r)
    case Lit(v) => v
  }

  val result = eval(Add(Lit(2), Div(Mul(Lit(5), Lit(2)), Lit(3))))
  println(s"""The result of Add(Lit(2), Div(Mul(Lit(5), Lit(2)), Lit(3))) ==> ${result}""")
}
