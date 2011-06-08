import org.specs2.mutable._
import org.specs2.specification._

class CodeBreakerSpec extends Specification {
  
  trait context extends Scope {
    val game = new CodeBreaker("r y g b")
  }

  "The Code Breaker" should {

    "return an empty list if none match" in new context {
      game.guess("a k j l") must be equalTo(List())
      game.guess("a k j l") must be equalTo(List())
    }

    "return an list with one 'p' if one match on right position " in new context {
      game.guess("r a j l") must be equalTo(List("p"))
    }

    "return an list with two 'p' if two match on right position " in new context {
      game.guess("r a g l") must be equalTo(List("p", "p"))
      game.guess("r a x b") must be equalTo(List("p", "p"))
      game.guess("r y x l") must be equalTo(List("p", "p"))
      game.guess("x y x b") must be equalTo(List("p", "p"))
    }


    "return an list with four 'p' if all match on right position " in new context {
      game.guess("r y g b") must be equalTo(List("p", "p", "p", "p"))
    }

    "return an list with one 'm' if one match on wrong position " in new context {
      game.guess("x r j l") must be equalTo(List("m"))
    }

    "return an list with two 'm' if two match on wrong position " in new context {
      game.guess("x r j g") must be equalTo(List("m", "m"))
    }


    "return an list with one 'p and one 'm'" in new context {
      game.guess("r x j g") must be equalTo(List("p", "m"))
    }

    "find both position and non-position matches" in new context {
      val game2 = new CodeBreaker("r y r b")

      game2.guess("r r x x") must be equalTo(List("p", "m"))
    }

    "handle multiple non-position matches" in new context {
      val game2 = new CodeBreaker("r y r b")

      game2.guess("r r x r") must be equalTo(List("p", "m", "m"))
    }
  }

}
