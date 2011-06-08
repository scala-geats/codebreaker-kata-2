


class CodeBreaker(secret: String) {

  val secretList = secret.split("\\s")

  def guess(guess: String): List[String] = {
    val guessList = guess.split("\\s")

    var matches = List[String]()
    guessList zip secretList foreach(i => {
      val (guessToken, secretToken) = i

      if(guessToken == secretToken) matches = "p" :: matches
      else {
        var found = false
        (0 until secretList.size).foreach(j => {
          if(secretList(j) == guessToken ) {
            found = true
          }
        })
        if(found) matches = "m" :: matches
      }
    })



    return matches.sortWith((s1, s2) => s1 > s2)
  }



}