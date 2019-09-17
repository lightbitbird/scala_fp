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

  println(Parser("true"))
  println(Parser("false"))
  println(Parser("fuga"))
  println(CombineParser("[]"))
  println(CombineParser("mmm"))
  println(MapParser("1"))

}
