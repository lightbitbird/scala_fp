package collection

object IteratorSample extends App {
  //Iterator は 2度と参照できないという欠点はあるが， 巨大なサイズのデータでも
  // List や Seq と同様に処理が可能になるという利点がある．
  val iter = Iterator(5, 3, 2)
  println(s"""iter.size = ${iter.size}""")
  println(s"""iter.size = ${iter.size}""")

  //Permutations
  println("順列 - Permutations: ")
  List(1,2,3).permutations.foreach(println)

  println("組合せ - Combinations: ")
  List(1,2,3).combinations(2).foreach(println)

  println("グループ化 - grouped: ")
  (1 to 10).grouped(3).foreach(println)
  println("sliding: ")
  (1 to 10).sliding(3).foreach(println)

}
