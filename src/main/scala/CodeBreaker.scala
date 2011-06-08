


class CodeBreaker(secret: String) {

  val secretList = secret.split("\\s")

  def guess(guess: String): List[String] = {
    val guessList = guess.split("\\s")

    var pMatches: Array[String] = guessList zip secretList map(i => {
      if(i._1 == i._2) "p"
      else ""
    })

    pMatches = pMatches.filter(_ == "p")

    var mMatches: Array[String] = guessList zip secretList map(i => {
      if(i._1 != i._2 && secretList.contains(i._1)) "m"
      else ""
    })

    mMatches = mMatches.filter(_ == "m")

    return (pMatches ++ mMatches).toList
  }



}