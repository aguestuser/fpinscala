package fpinscala.laziness

import org.scalatest.{Matchers, WordSpec}
import Stream._

/**
  * Created by aguestuser on 4/24/16.
  */
class StreamSpec extends WordSpec with Matchers {

  // 5.1

  "#toList" should {
    "force a Stream to a List" in {
      Stream(1,2,3).toList shouldEqual List(1,2,3)
      Stream(1,2,3).toListSafe shouldEqual List(1,2,3)
    }
  }

  // 5.2

  "#take" should {
    "produce a stream composed of the first n elements of a stream" in {
      Stream(1,2,3,4).take(2).toList shouldBe List(1,2)
      Stream(1,2,3,4).take(5).toList shouldBe List(1,2,3,4)
      Empty.take(5).toList shouldBe Nil
    }
  }

  // 5.3

  "#drop" should {
    "drop the first N elements from a Stream" in {
      Stream(1,2,3,4).drop(2).toList shouldBe List(3,4)
    }
  }

  // 5.4

  "#forAll" should {
    "return true if all elements of a stream satisfy a predicate, otherwise false" in {
      Stream(1,2,3,4) forAll (_ < 5) shouldBe true
      Stream(1,2,3,4) forAll (_ < 3) shouldBe false
    }
  }

  // 5.5

  "#takeWhile" should {
    "take the first N elements of a stream that satisfy a predicate" in {
      Stream(1,2,3,4).takeWhile(_ < 3).toList shouldBe List(1,2)
    }
  }


  // 5.6

  "#headOption" when {

    "the stream has a head" should {
      "return Some head" in {
        Stream(1,2,3).headOption shouldEqual Some(1)
      }
    }

    "the stream is empty" should {
      "return None"  in {
        Stream.empty[Int].headOption shouldEqual None
      }
    }

    // 5.7

    "#map" should {
      "map a function over a stream" in {
        Stream(1,2,3).map(_ + 1).toList shouldEqual List(2,3,4)
        Stream.empty[Int].map(_ + 1) shouldEqual Stream.empty[Int]
      }
    }

    "#filter" should {
      "filter a stream basedon a predicate" in {
        Stream(1,2,3).filter(_ % 2 == 0).toList shouldEqual List(2)
        Stream(1,2,3).filter(_ > 4) shouldEqual Stream.empty[Int]
      }
    }

    "#append" should {
      "append a stream to another stream" in {
        Stream(1,2,3).append(Stream(4,5,6)).toList shouldEqual List(1,2,3,4,5,6)
      }
    }

    "#flatMap" should {
      "flatMap a function over a stream" in {
        def triple[A](a: A): Stream[A] = Stream(a,a,a)
        Stream(1,2,3).flatMap(triple).toList shouldEqual List(1,1,1,2,2,2,3,3,3)
      }
    }

    // 5.8
    "#constant" should {
      "create an infinite steam of constant values" in {
        Stream.constant(1).take(3).toList shouldEqual List(1,1,1)
        Stream.constant("foo").take(3).toList shouldEqual List("foo", "foo", "foo")
      }
    }

    // 5.9
    "#from" should {
      "generate an infinite stream of integers incrementing by one" in {
       Stream.from(1).take(5).toList shouldEqual List(1,2,3,4,5)
      }
    }

    // 5.10
    "#fibs" should {
      "generate an infinite steam of fibonacci numbers" in {
        Stream.fibs.take(6).toList shouldEqual List(0,1,1,2,3,5)
      }
    }

    // 5.11, 5.12

    "#unfold" should {

      "generate #ones" in {
        onesViaUnfold.take(3).toList shouldEqual List(1,1,1)
      }

      "generate #constant" in {
        constantViaUnfold(1).take(3).toList shouldEqual List(1,1,1)
        constantViaUnfold("foo").take(3).toList shouldEqual List("foo", "foo", "foo")
      }

      "generate #from" in {
        fromViaUnfold(1).take(3).toList shouldEqual List(1,2,3)
      }

      "generate #fibs" in {
        fibsViaUnfold.take(6).toList shouldEqual List(0,1,1,2,3,5)
      }
    }

    // 5.13

    "#mapViaUnfold" should {

      "map a function over an infinite stream" in {
        Stream(1,2,3).mapViaUnfold(_ + 1).toList shouldEqual List(2,3,4)
      }
    }

    "#takeViaUnfold" should {

      "take N elements from a stream" in {
        Stream(1,2,3,4,5).takeViaUnfold(3).toList shouldEqual List(1,2,3)
      }
    }

    "#takeWhileViaUnfold" should {

      "consume elements from a stream while a predicate is satisfied" in {
        Stream(1,2,3,4,5).takeWhileViaUnfold(_ < 4).toList shouldEqual List(1,2,3)
      }
    }

    "#zipWith" should {

      "combine two streams by passing their heads pairwise to a 2-arity function" in {
        Stream(1,2,3,4).zipWith(Stream(4,3,2,1))(_ + _).toList shouldEqual List(5,5,5,5)
      }
    }

    "#zip" should {
      "interleave the elements of two streams into a stream of pairs" in {
        Stream(1,2,3,4).zip(Stream(4,3,2,1)).toList shouldEqual List((1,4),(2,3),(3,2),(4,1))
      }
    }

    "#zipAll" should {

      "interleave the elements of two streams until both streams are exhausted" in {
        Stream(1,2).zipAll(Stream(1,2,3)).toList shouldEqual
          List( (Some(1),Some(1)), (Some(2),Some(2)), (None,Some(3)) )
      }
    }

    // 5.14

    "#startsWith" should {

      "return true" when {
        "a stream begins with another stream" in {
          Stream(1,2,3,4).startsWith(Stream(1,2,3)) shouldBe true

        }
      }

      "return false" when {
        "a stream does not begin with another stream" in {
          Stream(1,2,3,4).startsWith(Stream(4,5,6)) shouldBe false
        }
      }
    }

    // 5.15

    "#tails" should {

      "return all the suffixes of a stream" in {
        Stream(1,2,3,4).tails.toList.map(_.toList) shouldEqual
          List(List(1,2,3,4), List(2,3,4), List(3,4), List(4), Nil)
      }
    }

    "#hasSubsequence" should {

      "return true" when {
        "a stream has another stream as a subsequence" in {
          Stream(1,2,3,4).hasSubsequence(Stream(3,4)) shouldBe true
        }
      }

      "return false" when {
        "a stream doesn't have another stream as a subsequence" in {
          Stream(1,2,3,4).hasSubsequence(Stream(1,4)) shouldBe false
        }
      }
    }

    // 5.16

    "#scanRight" should {
      "return a stream the results of a folding a function over a stream at each step in the fold" in {
        Stream(1,2,3,4).scanRight(0)(_ + _).toList shouldEqual List(10,9,7,4,0)
      }
    }
  }
}
