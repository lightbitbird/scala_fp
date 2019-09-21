package parsers

object JsonParserMain extends App {

  println()
  println(JSONParser("[1.0,-2.0,true,false,null,{\"hoge\":\"fuga\",\"piyo\":null}]"))
  println(JSONParser("[1.0, -2.0, true, false, null, {\"hoge\" : \"fuga\", \"piyo\" : null}] "))
  println(KeyWordParser("true"))
  println(KeyWordParser("false"))
  println(KeyWordParser("fuga"))
  println(CombineParser("[]"))
  println(CombineParser("mmm"))
  println(MapParser("1"))
  println(DisposeOneParser("[true]"))
  println(RepsepParser("true,true,true"))
  println(FloatParser("-3.14"))
  println(StringParser("\"hoge\""))
  println(ArithParser("'(5.1+3*6.2)*5/3.4-2"))

}

/**
 * <value> ::= <obj> | <arr> | <stringLiteral> | <floatingPointNumber> | "null" | "true" | "false"
 * <obj> ::= "{" [<members>] "}"
 * <arr> ::= "[" [<values>] "]"
 * <members> ::= <member> { "," <member> }
 * <member> ::= <stringLiteral> ":" <value>
 * <values> ::= <value> { "," <value> }<value> ::= <obj> | <arr> | <stringLiteral> | <floatingPointNumber> | "null" | "true" | "false"
 * <obj> ::= "{" [<members>] "}"
 * <arr> ::= "[" [<values>] "]"
 * <members> ::= <member> { "," <member> }
 * <member> ::= <stringLiteral> ":" <value>
 * <values> ::= <value> { "," <value> }
 */
object JSONParser extends Combinator {
  def apply(input: String): Any = value(input)

  def obj: Parser[Map[String, Any]] =
    (ss("{") ~> repsep(member, ss(",")) <~ ss("}") ^^ {
      Map() ++ _
    }) <~ spacing

  def arr: Parser[List[Any]] =
    (ss("[") ~> repsep(value, ss(",")) <~ ss("]")) <~ spacing

  def member: Parser[(String, Any)] =
    ((stringLiteral <~ spacing) ~ ss(":") ~ value ^^ { t => (t._1._1, t._2) }) <~ spacing

  def value: Parser[Any] =
    obj <~ spacing | arr <~ spacing | stringLiteral <~ spacing |
      (floatingPointNumber ^^ {
        _.toDouble
      }) <~ spacing |
      ss("null") ^^ { _ => null } |
      ss("true") ^^ { _ => true } |
      ss("false") ^^ { _ => false }
}

object ArithParser extends Combinator {

  def apply(input: String): Any = expr(input)

  def expr: Parser[Any] = term ~ rep(s("+") ~ term | s("-") ~ term)

  def term: Parser[Any] = factor ~ rep(s("*") ~ factor | s("/") ~ factor)

  def factor: Parser[Any] = floatingPointNumber | s("(") ~ expr ~ s(")")
}

