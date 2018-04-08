package collection

object ListSample extends App {

  val anyList = List("Scala", 2.8)
  anyList.foreach(println)

  val rangeList = Range(2, 13).toList
  val rangeList2 = List.range(2, 7, 2)
  rangeList2.foreach(a => println(s"""2, 7, 2 => ${a}"""))

  val list = List(6, 4, 17, 13)
  println(s"""list = ${list}""")
  println(s"""head = ${list.head}""")
  println(s"""tail = ${list.tail}""")
  println(s"""3 :: list = ${3 :: list}""")
  println(s"""3 +: list = ${3 +: list}""")
  println(s"""last = ${list.last}""")
  println(s"""init = ${list.init}""")
  println(s"""list.apply(3) = ${list.apply(3)}""")
  println(s"""list.take(2) = ${list.take(2)}""")
  println(s"""list.takeRight(3) = ${list.takeRight(3)}""")
  println(s"""list.drop(2) = ${list.drop(2)}""")
  println(s"""list.dropRight(3) = ${list.dropRight(3)}""")
  println(s"""list.contains(17) = ${list.contains(17)}""")
  println(s"""list ::: list = ${list ::: List(5, 21, 30)}""")
  println(s"""list.reverse = ${list.reverse}""")
  println(s"""list.sorted = ${list.sorted}""")
  println(s"""list.sum = ${list.sum}""")
  println(s"""list.product = ${list.product}""")
  println(s"""list.max = ${list.max}""")
  println(s"""list.min = ${list.min}""")
  println(s"""List.fill(4)(1) = ${List.fill(4)(1)}""")
  println(s"""average = ${average(list)}""")

  //Average
  def average(list: List[Int]) = list.sum / list.size

  //順列の個数nPr
  def permutation(n: Int, r: Int) = (BigInt(n - r + 1) to n).product
  println(permutation(100, 20))

  //ひとつ左へ回転
  def rotateLeft[T](list: List[T]): List[T] = list.tail :+ list.head
  //ひとつ右へ回転
  def rotateRight[T](list: List[T]): List[T] = list.last +: list.init

  val list2 = List(7, 2, 5, 9, 3)
  list2.map(_ + 1)
  def inc(x: Int): Int = x + 1
  list2.map(inc)
  list2.foreach(x => println(s"""${x + 1}"""))

  val forall = list2.forall(_ > 2)
  println(s"""forall = ${forall.toString}""")
  println(s"""exists = ${list2.exists(_ > 2)}""")
  println(s"""exists = ${list.indexWhere(_ > 2)}""")
  println(s"""exists = ${list.indexWhere(_ > 10)}""")

  //sortWith(f)
  println(s"""sortWith: a > b = ${list2.sortWith((a, b) => a > b)}""")
  println(s"""sortWith: _ > _ = ${list2.sortWith(_ > _)}""")

  //sortBy(f)
  println(s"""sortBy: a => -a = ${list2.sortBy(a => -a)}""")

  //reduceLeft
  println(s"""reduceLeft: (a, b) => a + b : ${list2.reduceLeft((a, b) => a + b)}""")
  println(s"""reduceLeft: (a, b) => 10*a + b : ${list2.reduceLeft((a, b) => 10*a + b)}""")
  println(s"""reduceLeft: 10*_ + _ : ${list2.reduceLeft(10*_ + _)}""")

  //reduceRight
  println(s"""reduceRight: (a, b) => a + b : ${list2.reduceRight((a, b) => a + b)}""")
  println(s"""reduceRight: (a, b) => a + 10*b : ${list2.reduceRight((a, b) => a + 10*b)}""")
  println(s"""reduceRight: _ + 10*_ : ${list2.reduceRight(_ + 10*_)}""")

  //foldLeft
  println(s"""foldLeft(0)(_ + _) : ${list2.foldLeft(0)(_ + _)}""")
  println(s"""foldLeft(0)(10*_ + _) : ${list2.foldLeft(0)(10*_ + _)}""")

  //foldRight
  println(s"""foldRight(0)(_ + _) : ${list2.foldRight(0)(_ + _)}""")
  println(s"""foldRight(0)(_ + 10*_) : ${list2.foldRight(0)(_ + 10*_)}""")

  //list.map
  val lists = List(List(3, 1, 4, 1), List(2, 7, 1, 8), List(0, 5, 7, 7))
  println(s"""lists.map: ${lists.map(_.filter(_ % 2 != 0))}""")
  println(s"""lists.map().reduceLeft: ${lists.map(_.filter(_ % 2 != 0)).reduceLeft(_ ++ _)}""")

  //list.flatMap
  println(s"""lists.flatMap: ${lists.flatMap(_.filter(_ % 2 != 0))}""")
  println(s"""lists.flatMap(x => x): ${lists.flatMap(x => x)}""")

  //flatMap + map
  println(
    s"""(1 to 3).flatMap(i => (1 to 3).map(j => 10*i + j):
       |${(1 to 3).flatMap(i => (1 to 3).map(j => 10*i + j))}""".stripMargin)

  //for
  val forList = for {
    i <- 1 to 3
    j <- 1 to 3
  } yield (10 * i + j)
  println(s"""for flatMap: ${forList}""")


}
