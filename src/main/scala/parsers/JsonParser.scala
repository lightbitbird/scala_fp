package parsers

object JsonParser extends App {

  object Parser extends Combinator {
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
    def apply(input: String): ParseResult[List[String]] = repsep(s("true"), s(",")) (input)
  }

  object FloatParser extends Combinator {
    def apply(input: String): ParseResult[String] = floatingPointNumber(input)
  }

  object StringParser extends Combinator {
    def apply(input: String): ParseResult[String] = stringLiteral(input)
  }

  println(Parser("true"))
  println(Parser("false"))
  println(Parser("fuga"))
  println(CombineParser("[]"))
  println(CombineParser("mmm"))
  println(MapParser("1"))
  println(DisposeOneParser("[true]"))
  println(RepsepParser("true,true,true"))
  println(FloatParser("-3.14"))
  println(StringParser("\"hoge\""))

}
