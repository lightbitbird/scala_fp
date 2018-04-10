package others

object SurrogatePairs {
  def main(args: Array[String]): Unit = {
    println(s"length ==> ${"𠹭タロま𤄃".length}")
    println(s"codePointCount ==> ${SurrogatePairsUtil.spLength("𠹭タロま")}")
    println(s"offsetByCodePoints ==> ${"𠹭タロま𤄃".offsetByCodePoints(0, 3)}")
    println(s"diffCount ==> ${SurrogatePairsUtil.getDiffCount("𠹭タロま𤄃", 4, 2)}")

    println(s"Surrogate substring ==> ${SurrogatePairsUtil.subString("𠹭タロま𤄃", 0, 3)}")
    println(s"Surrogate substring ==> ${SurrogatePairsUtil.substring("𠹭タロま𤄃", 0, 6)}")
    println(s"Surrogate codePointSubstring ==> ${SurrogatePairsUtil.substringCodePoint("𠹭タロま𤄃", 0, 6)}")
  }

}

object SurrogatePairsUtil {

  def spLength(string: String) = {
    string.codePointCount(0, string.length)
  }

  def substring(target: String, start: Int, end: Int): String = {
    val offsetEnd = target.offsetByCodePoints(0, end-1)
    val diff = getDiffCount(target, end - 1, offsetEnd)
    val charArray = target.toCharArray
    val codePoint = Character.codePointAt(charArray, end - 1)
    val char = Character.toChars(codePoint)

    if (Character.isHighSurrogate(char(0))) {
      println(s"char(0): HighSurrogate ==> ${char(0)}")
      if (Character.isLowSurrogate(char(1))) {
        println(s"char(1): LowSurrogate ==> ${char(1)}")
      }
//      sb.append(String.valueOf(Character.toChars(codePoint)))
    } else {
      println(s"char(0): HighSurrogate ==> ${char(0)}")
    }
    target.substring(start, end - 2)
  }

  def getDiffCount(target: String, end: Int, offsetEnd: Int) = {
    if (end > offsetEnd) {
      println("end - offsetEnd => " + (end - offsetEnd))
      target.offsetByCodePoints(offsetEnd, end - offsetEnd)
    }
    else
      offsetEnd
  }

  def substringCodePoint(target: String, start: Int, end: Int): String = {
    val charArray = target.toCharArray

    val endIndex = {
      if (target.length >= end) end - 1
      else target.length - 1
    }
//    val offsetStart = target.offsetByCodePoints(0, start)
//    val offsetEnd = target.offsetByCodePoints(0, endIndex)
    val sb = new StringBuffer
    var count = 0
    for (i <- start until endIndex) {
      val codePoint = Character.codePointAt(charArray, count)
      val char = Character.toChars(codePoint)
      if (Character.isHighSurrogate(char(0))) {
        sb.append(String.valueOf(char))
        println(s"highSurrogate --> ${i}")
        count += 2
      } else {
        if (!Character.isLowSurrogate(char(0))) {
          sb.append(String.valueOf(charArray(count)))
          println(s"normal --> ${i}")
          count += 1
        }
      }
//      i += Character.charCount(codePoint)
    }
    sb.toString
  }

  def subString(target: String, startIndex: Int, endIndex: Int): String = {
    // char配列の取得
    val charArray = target.toCharArray
    // コードポイント分だけの開始位置インデックスの取得（繰り返し用）
    val offsetStart = target.offsetByCodePoints(0, startIndex)
    // コードポイント分だけの終了位置インデックスの取得（繰り返し用）
    val offsetEnd = target.offsetByCodePoints(0, endIndex)
    // コードポイント初期値
    var codePoint = 0
    val sb = new StringBuffer
    var i = offsetStart
    while ( {
      i < offsetEnd
    }) { // カレント文字のコードポイントの取得
      codePoint = Character.codePointAt(charArray, i)
      sb.append(String.valueOf(Character.toChars(codePoint)))

      i += Character.charCount(codePoint)
    }
    sb.toString
  }
}