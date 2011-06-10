
class CodeBreaker(secret: String) {

  private val secrets = secret.split(" ")

  def ?(guess: String) = {
    // produces two list of tuples, one with position matches, and the rest
    val (ps, others) = guess.split(" ") zip secrets partition {case (g,s) => g == s }

    // splits the tuples into two separate lists
    val (sr, gr) = others unzip

    // intersects (returns only those elements which exists in both lists)
    // then maps to "p" or "m"
    (ps map(t => "p")) ++ (sr intersect gr map(t => "m"))
  }
}

