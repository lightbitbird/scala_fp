package others

object SurrogatePairs {
  def main(args: Array[String]): Unit = {
    println(s"length ==> ${"𠹭タロま𤄃".length}")
    println(s"codePointCount ==> ${SurrogatePairsUtil.codePointCount("𠹭タロま")}")
    println(s"offsetByCodePoints ==> ${"𠹭タロま𤄃".offsetByCodePoints(0, 3)}")

    println(s"Surrogate substrByCodePointCount(3, 6) ==> ${SurrogatePairsUtil.substrByCodePointCount("𠹭タロま𤄃a", 3, 6)}")
    println(s"Surrogate substrByCodePointCount(0, 10) ==> ${SurrogatePairsUtil.substrByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 0, 10)}")
    println(s"Surrogate substrByCodePointCount(2, 5) ==> ${SurrogatePairsUtil.substrByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 2, 5)}")

    println(s"-------------------------------------------")

    println(s"Surrogate sliceByCodePointCount ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃a", 3, 6)}")
    println(s"Surrogate sliceByCodePointCount(0, 10) ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 0, 10)}")
    println(s"Surrogate sliceByCodePointCount(2, 7) ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 2, 7)}")
    println(s"Surrogate sliceByCodePointCount(5, 11) ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 5, 11)}")
    println(s"Surrogate sliceByCodePointCount(-5, 11) ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", -5, 11)}")
    println(s"Surrogate sliceByCodePointCount(5, -3) ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 5, -3)}")
  }

}

object SurrogatePairsUtil {

  def codePointCount(string: String) = {
    string.codePointCount(0, string.length)
  }

  def substrByCodePointCount(target: String, from: Int, len: Int): String = {
    val charArray = target.toCharArray
    val cpCount = codePointCount(target)
    val start = {
      if (target.isEmpty || cpCount < from) 0
      else target.offsetByCodePoints(0, from)
    }
    val count = {
      if (target.isEmpty || cpCount < from) 0
      else if (cpCount >= from + len) start + len
      else cpCount - from + start
    }
    var index = start
    (start until count).foldLeft("") { (concat, i) =>
      val codePoint = Character.codePointAt(charArray, index)
      index += Character.charCount(codePoint)
      concat + String.valueOf(Character.toChars(codePoint))
    }
  }

  def sliceByCodePointCount(target: String, from: Int, until: Int): String = {
    val len = codePointCount(target)
    val f = target.offsetByCodePoints(0, Math.max(Math.max(0, from), 0))
    val u = target.offsetByCodePoints(0, Math.min(Math.max(0, until), len))
    target.slice(f, u)
  }
}