package others

object SurrogatePairs {
  def main(args: Array[String]): Unit = {
    println(s"length ==> ${"𠹭タロま𤄃".length}")
    println(s"codePointCount ==> ${SurrogatePairsUtil.codePointCount("𠹭タロま")}")
    println(s"offsetByCodePoints ==> ${"𠹭タロま𤄃".offsetByCodePoints(0, 3)}")

    println(s"Surrogate sliceByCodePointCount ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃a", 3, 6)}")
    println(s"Surrogate sliceByCodePointCount2 ==> ${SurrogatePairsUtil.sliceByCodePointCount("𠹭タロま𤄃aXX𤄃Xbbb", 0, 10)}")
  }

}

object SurrogatePairsUtil {

  def codePointCount(string: String) = {
    string.codePointCount(0, string.length)
  }

  def sliceByCodePointCount(target: String, from: Int, len: Int): String = {
    val charArray = target.toCharArray
    val start = target.offsetByCodePoints(0, from)
    val count = {
      if (codePointCount(target) >= from + len) start + len
      else codePointCount(target) - from + start
    }
    var index = start
    (start until count).foldLeft("") { (concat, i) =>
      val codePoint = Character.codePointAt(charArray, index)
      index += Character.charCount(codePoint)
      concat + String.valueOf(Character.toChars(codePoint))
    }
  }
}