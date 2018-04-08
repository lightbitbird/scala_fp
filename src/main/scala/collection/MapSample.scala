package collection

object MapSample extends App {

  var map = Map("hamlet" -> 108, "ophelia" -> 32, "katie" -> 57)
  println(s"""map = ${map}""")
  println(s"""map + ("horatio" -> 48) : ${map + ("horatio" -> 48)}""")
  map = map + ("horatio" -> 48)
  println(s"""map + ("horatio" -> 48) : ${map}""")
  map += ("horatio" -> 48)
  println(s"""map += ("horatio" -> 48) : ${map}""")
  println(s"""contains: ${map.contains("hamlet")}""")
  println(s"""apply: ${map.apply("hamlet")}""")
  println(s"""map("hamlet"): ${map("hamlet")}""")
  println(s"""map.get("hamlet"): ${map.get("hamlet")}""")
  println(s"""map.get("king"): ${map.get("king")}""")
  println(s"""map.getOrElse("hamlet", 0): ${map.getOrElse("hamlet", 0)}""")
  println(s"""map.getOrElse("king", 0): ${map.getOrElse("king", 0)}""")
  println(s"""map.keys: ${map.keys}""")
  println(s"""map.filterKeys: ${map.filterKeys(_.length >= 7)}""")
  println(s"""map.filter(kv => kv._2 >= 40): ${map.filter(_._2 >= 40)}""")
  println(s"""map.filter{case (k, v) => v >= 40}: ${map.filter{case (k, v) => v >= 40}}""")
  val values = for (kv <- map; if kv._2 >= 60) yield kv
  println(s"""for (kv <- map; if kv._2 >= 40) yield kv: ${values}""")


}
