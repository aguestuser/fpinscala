package fpinscala.errorhandling

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by aguestuser on 4/23/16.
  */
class EitherSpec extends WordSpec with Matchers {

  import Either._

  def isEven(n: Int): Boolean = n % 2 == 0
  def doubleIfEven(n:Int): Either[String,Int] = if (isEven(n)) Right(2*n) else Left("Odd number encountered!")
  def add(m: Int, n: Int): Int = m + n

  "map" should {
    "map a function over an Either" in {
      Right(1) map (_ + 1) shouldEqual Right(2)
      Left("error") map[Int]((x:Int) => x + 1) shouldEqual Left("error")
    }
  }

  "flatMap" should {
    "flatmap a function over an Either" in {
      Right(2) flatMap doubleIfEven shouldEqual Right(4)
      Right(1) flatMap doubleIfEven shouldEqual Left("Odd number encountered!")
      Left("foobar") flatMap doubleIfEven shouldEqual Left("foobar")
    }
  }

  "orElse" should {
    Right(1) orElse Right(2) shouldEqual Right(1)
    Left("error") orElse Right(2) shouldEqual Right(2)
    Left("error") orElse Left("new error") shouldEqual Left("new error")
  }

  "map2" should {
    "lift a function with arity 2 into an Either context" in {
      Right(1).map2(Right(2))(add) shouldEqual Right(3)
      Left("error").map2(Right(2))(add) shouldEqual Left("error")
      Left("error").map2(Left("other error"))(add) shouldEqual Left("error")
    }
  }

  "#traverse" should {
    "map a partial function over a list, returning a Right of a List of results if fn defined for all members, a Left of an error if not" in {
      traverse[String,Int,Int](List(2,4,6))(doubleIfEven) shouldEqual Right(List(4,8,12))
      traverse[String,Int,Int](List(1,2,3))(doubleIfEven) shouldEqual Left("Odd number encountered!")
    }
  }

  "#sequence" should {
    "convert a List of Eithers into an Either of an error or a List" in {
      sequence(List(Right(1), Right(2), Right(3))) shouldEqual Right(List(1,2,3))
      sequence(List(Right(1), Left("whoops!"), Left("oh noes!"))) shouldEqual Left("whoops!")
    }
  }
}
