


class CodeBreaker(secret: String) {

  val secretList = secret.split("\\s")

  def guess(guess: String): List[String] = {
    val guessList = guess.split("\\s")

    var matches = List[String]()
    (0 until guessList.size).foreach(i => {
      val guessToken = guessList(i)
      val secretToken = secretList(i)

      //println(guessToken + " -- " + secretToken)
      //println(guessToken == secretToken)
      if(guessToken == secretToken) matches = "p" :: matches
      else {
        var found = false
        (0 until secretList.size).foreach(j => {
          if(secretList(j) == guessToken && i != j) {
            found = true
          }
        })
        if(found) matches = "m" :: matches
      }
    })



    return matches.sortWith((s1, s2) => s1 > s2)
  }



}