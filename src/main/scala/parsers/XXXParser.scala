package parsers

case class PostalCode(zoneCode: String, townCode: String)

object KeyWordParser extends Combinator {
  def apply(input: String): ParseResult[String] = (s("true") | s("false")) (input)
}

object CombineParser extends Combinator {
  def apply(input: String): ParseResult[(String, String)] = (s("[") ~ s("]")) (input)
}

object MapParser extends Combinator {
  def apply(input: String): ParseResult[Int] = (s("1") ^^ {
    _.toInt
  }) (input)
}

object DisposeOneParser extends Combinator {
  def apply(input: String): ParseResult[String] = (s("[") ~> s("true") <~ s("]")) (input)
}

object RepsepParser extends Combinator {
  def apply(input: String): ParseResult[List[String]] = repsep(s("true"), s(","))(input)
}

object FloatParser extends Combinator {
  def apply(input: String): ParseResult[String] = floatingPointNumber(input)
}

object StringParser extends Combinator {
  def apply(input: String): ParseResult[String] = stringLiteral(input)
}

object PostalCodeParser extends AbstractXXXCombinator {
  def digit: Parser[String] = oneOf('0' to '9')

  def zoneCode: Parser[String] = map(combine(combine(digit, digit), digit), {
    t: ((String, String), String) => t._1._1 + t._1._2 + t._2
  })

  def twoCode: Parser[String] = map(combine(combine(combine(digit, digit), digit), digit), {
    t: (((String, String), String), String) => t._1._1._1 + t._1._1._2 + t._1._2 + t._2
  })

  def apply(input: String): ParseResult[PostalCode] = map(combine(combine(zoneCode, s("-")), twoCode), {
    t: ((String, String), String) => PostalCode(t._1._1, t._2)
  })(input)

}

case class FullClassName(grade: String, className: String)

object FullClassNameParser extends AbstractXXXCombinator {
  def digit: Parser[String] = oneOf('1' to '3')

  def className: Parser[String] = oneOf('A' to 'D')

  def apply(input: String): ParseResult[FullClassName] = map(combine(combine(combine(digit, s("grade")), className), s("class")), {
    t: (((String, String), String), String) => FullClassName(t._1._1._1, t._1._2)
  })(input)
}

object BooleanParser extends AbstractXXXCombinator {
  def trueParser: Parser[Boolean] = map(s("true"), { _: String => true })

  def falseParser: Parser[Boolean] = map(s("false"), { _: String => false })

  def apply(input: String): ParseResult[Boolean] =
    select(trueParser, falseParser)(input)
}
