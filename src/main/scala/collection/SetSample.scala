package collection

object SetSample extends App {
  val set = Set(2, 7, 1, 8, 2, 8)
  println(s"""set: ${set}""")
  println(s"""intersect: ${set.intersect(Set(2, 1, 4))}""")
  println(s"""&: ${set & Set(2, 1, 4)}""")
  println(s"""union: ${set.union(Set(3, 1, 4))}""")
  println(s"""diff: ${set.diff(Set(3, 1, 4))}""")

  println(s"""subsetOf: ${set.subsetOf(Set(3, 1, 4))}""")
  println(s"""subsetOf: ${set.subsetOf(Set(2, 7, 1, 8))}""")

}
