
class CodeBreaker(secret: String) {

  private val secrets = secret.split(" ")

  def ?(guess: String) = {
    val zipped = guess.split(" ") zip secrets

    (for((g, s) <- zipped if g == s) yield "p") ++
      (for((g, s) <- zipped if g != s && secrets.contains(g)) yield "m")
  }
}

