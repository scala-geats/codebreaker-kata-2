import org.specs2.mutable._
import org.specs2.specification._

class CodeBreakerSpec extends Specification {
  
  trait context extends Scope {

  }

  implicit def String2CodeBreaker(secret: String): CodeBreaker = new CodeBreaker(secret)

  "The Code Breaker" should {

    "return an empty list if none match" in new context {
      ("r y g b" ? "a k j l").toList must be equalTo(List())
      ("r y g b" ? "a k j l").toList must be equalTo(List())
    }

    "return an list with one 'p' if one match on right position " in new context {
      ("r y g b" ? "r a j l").toList must be equalTo(List("p"))
    }

    "return an list with two 'p' if two match on right position " in new context {
      ("r y g b" ? "r a g l").toList must be equalTo(List("p", "p"))
      ("r y g b" ? "r a x b").toList must be equalTo(List("p", "p"))
      ("r y g b" ? "r y x l").toList must be equalTo(List("p", "p"))
      ("r y g b" ? "x y x b").toList must be equalTo(List("p", "p"))
    }


    "return an list with four 'p' if all match on right position " in new context {
      ("r y g b" ? "r y g b").toList must be equalTo(List("p", "p", "p", "p"))
    }

    "return an list with one 'm' if one match on wrong position " in new context {
      ("r y g b" ? "x r j l").toList must be equalTo(List("m"))
    }

    "return an list with two 'm' if two match on wrong position " in new context {
      ("r y g b" ? "x r j g").toList must be equalTo(List("m", "m"))
    }


    "return an list with one 'p and one 'm'" in new context {
      ("r y g b" ? "r x j g").toList must be equalTo(List("p", "m"))
    }

    "find both position and non-position matches" in new context {
      ("r y r b" ? "r r x x").toList must be equalTo(List("p", "m"))
    }

    "handle multiple non-position matches, but only until matches are used up" in new context {
      ("r y r b" ? "r r x r").toList must be equalTo(List("p", "m"))
    }

    "not reuse already matched colors" in new context {
      ("r r y b" ? "r r r r").toList must be equalTo(List("p", "p"))
    }

  }

}
