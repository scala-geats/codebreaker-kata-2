


class CodeBreaker(secret: String) {

  val secretList = secret.split("\\s")

  def guess(guess: String): List[String] = {
    val zippedList = guess.split("\\s") zip secretList

    var pMatches = for(tuple <- zippedList if tuple._1 == tuple._2) yield "p"

    var mMatches = for(tuple <- zippedList if tuple._1 != tuple._2 && secretList.contains(tuple._1)) yield "m"

    return (pMatches ++ mMatches).toList
  }



}