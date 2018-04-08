package others

object SurrogatePairs {
  def main(args: Array[String]): Unit = {
    println(s"length ==> ${"𠹭タロま".length}")
    println(s"codePointCount ==> ${SurrogatePairsUtil.spLength("𠹭タロま")}")
    println(s"offsetByCodePoints ==> ${"𠹭タロま".offsetByCodePoints(0, 3)}")
    println(s"diffCount ==> ${SurrogatePairsUtil.getDiffCount("𠹭タロま", 4, 2)}")

    println(s"Surrogate substring ==> ${SurrogatePairsUtil.subString("𠹭タロま", 0, 3)}")
  }

}

object SurrogatePairsUtil {

  def spLength(string: String) = {
    string.codePointCount(0, string.length)
  }

  def substring(target: String, start: Int, end: Int): String = {
    val offsetEnd = target.offsetByCodePoints(0, end-1)
    val diff = getDiffCount(target, end - 1, offsetEnd)


    ""
  }

  def getDiffCount(target: String, end: Int, offsetEnd: Int) = {
    if (end > offsetEnd) {
      println("end - offsetEnd => " + (end - offsetEnd))
      target.offsetByCodePoints(offsetEnd, end - offsetEnd)
    }
    else
      offsetEnd
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