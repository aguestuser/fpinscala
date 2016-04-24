package fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}
import Option._

/**
  * Created by aguestuser on 4/22/16.
  */

class OptionSpec extends WordSpec with Matchers {

  def isEven(n: Int): Boolean = n % 2 == 0
  def maybeEven(n: Int): Option[Int] = if (isEven(n)) Some(n) else None

  "#map" should {
    "Map a function onto an Option" in {
      Some(1) map (_ + 1) shouldEqual Some(2)
      None map {n:Int => n + 1} shouldEqual None
    }
  }

  "#flatMap" should {
    "Flatmap a function onto an Option" in {
      Some(2) flatMap maybeEven shouldEqual Some(2)
      Some(1) flatMap maybeEven shouldEqual None
    }
  }


  "#getOrElse" should {
    "Return the contents of a Some or a default value for a None" in {
      Some(1) getOrElse 2 shouldEqual 1
      None getOrElse 2 shouldEqual 2
    }

  }

  "#orElse" should {
    "Return the first option if its defined, otherwise the second option" in {
      Some(1) orElse Some(2) shouldEqual Some(1)
      None orElse Some(2) shouldEqual Some(2)
    }
  }

  "#filter" should {
    "Return Some if a predicate is satisfied, otherwise None" in {
      Some(1).filter(isEven)shouldEqual None
      Some(2) filter isEven shouldEqual Some(2)
      None filter isEven shouldEqual None
    }
  }

  "#variance" should {
    "calculate the variance of a list or return None if undefined forinput" in {
      variance(Seq(1.0, 1.1, 0.9)) shouldEqual Some(0.0066666666666666706)
    }
  }

  "#map2" should {
    "lift a function with arity two into an Option context" in {
      def add(m: Int, n: Int): Int = m + n
      map2(Some(1), Some(2))(add) shouldEqual Some(3)
      map2(None, Some(1))(add) shouldEqual None
      map2(Some(1), None)(add) shouldEqual None
    }
  }

  "#sequence" should {
    "convert a List of Options into an Option of a List" in {
      sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1,2,3))
      sequence(List(Some(1), None)) shouldEqual None
    }
  }

  "#traverse" should {
    "perform #map and #sequence in a single traversal of a list (not two)" in {
      traverse(List(2,4,6))(maybeEven) shouldEqual Some(List(2,4,6))
      traverse(List(1,2,3))(maybeEven) shouldEqual None
    }
  }
}
