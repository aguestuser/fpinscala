package fpinscala.datastructures

import List._
import org.scalatest.{WordSpec, ShouldMatchers }

class ListSpec extends WordSpec with ShouldMatchers  {

  val nums = List(1,2,3,4,5)

  // 3.2

  "#tail" should {

    "drop the head from a list" in {
      tail(nums) shouldEqual List(2,3,4,5)
    }

    "return Nil if the list is Nil" in { // todo: how to test if we threw error?
      tail(Nil) shouldEqual Nil
    }
  }

  // 3.3

  "#setHead" should {

    "replace the head of a list" in {
      setHead(nums, 0) shouldEqual List(0,2,3,4,5)
    }
  }

  // 3.4

  "#drop" should {

    "remove the first n elements from a list" in {
      drop(nums, 2) shouldEqual List(3,4,5)
      drop(nums, 3) shouldEqual List(4,5)
      drop(nums, 5) shouldEqual Nil
    }
  }

  // 3.5

  "#dropWhile" should {

    "remove the first n elements that satisfy a predicate" in {
      dropWhile(nums)(_ < 3) shouldEqual List(3,4,5)
      dropWhile(nums)(_ % 2 == 1) shouldEqual List(2,3,4,5)
    }
  }

  // 3.6

  "#init" should {

    "drop the last element of a list" in {
      init(nums) shouldEqual List(1,2,3,4)
    }
  }

  // 3.9

  "#length" should {

    "return the length of a list" in {
      List.length(nums) shouldEqual 5
    }
  }

  // 3.10 - 3.11

  "#foldLeft" should {

    "sum a list" in {
      foldLeft(nums, 0)(_+_) shouldEqual 15
    }

    "multiply all elements in a list" in {
      foldLeft(nums, 1.0)(_*_) shouldEqual 120
    }

    "find the length of a list" in {
      foldLeft(nums, 0)((acc,_) => acc + 1) shouldEqual 5
      foldRight(nums, 0)((_,acc) => acc + 1) shouldEqual 5
    }
  }

  // 3.12

  "#reverse" should {

    "reverse a list in terms of a fold" in {
      reverse(nums) shouldEqual List(5,4,3,2,1)
    }
  }

  // 3.13

  "#foldRightViaFoldLeft" should {

    "find the length of a list -- with accumulator as second argument to folded function" in {
      foldRightViaFoldLeft(nums, 0)((_,acc) => acc + 1) shouldEqual 5
    }
  }

  "#foldLeftViaFoldRight" should {

    "find the length of a list -- with accum. as first arg to folded func" in {
      foldLeftViaFoldRight(nums, 0)((acc,_) => acc + 1) shouldEqual 5
    }
  }

  // 3.14

  "#appendViaFoldRigh" should {

    "append one list to another" in {
      appendViaFoldRight(nums, Cons(6, Cons(7, Nil))) shouldEqual List(1,2,3,4,5,6,7)
    }
  }

  "#appendViaFoldLeft" should {

    "append one list to another" in {
      appendViaFoldLeft(nums, List(6,7)) shouldEqual List(1,2,3,4,5,6,7)
    }
  }

  // 3.15

  "#concat" should {

    "concatenate many lists into one" in {
      concat(List(List(1,2), List(3,4), List(5,6))) shouldEqual List(1,2,3,4,5,6)
    }
  }

  // 3.16

  "#add1ViaFoldRight" should {

    "add 1 to every member of a list" in {
      add1ViaFoldRight(nums) shouldEqual List(2,3,4,5,6)
    }

  }

  "#add1ViaFoldLeft" should {

    "add 1 to every member of a list" in {
      add1ViaFoldLeft(nums) shouldEqual List(2,3,4,5,6)
    }

  }

  // 3.17

  "#stringifyViaFoldRigh" should {

    "stringify every member of a list" in {
      stringifyViaFoldRight(nums) shouldEqual List("1", "2", "3", "4", "5")
    }
  }

  // 3.18

  "#map" should {

    "map a function over a list in terms of fold" in {
      map(nums)(_ + 1) shouldEqual List(2,3,4,5,6)
    }
  }

  // 3.19

  "#filter" should {

    "filter elements of a list that don't match a predicate" in {
      filter(List(1,2,3,4,5))(_ % 2 == 0) shouldEqual List(2,4)
    }
  }

  // 3.20

  "#flatMap" should {

    "flatten several calls to map" in {
      flatMap(List(1,3,5))(a => List(a, a+1)) shouldEqual List(1,2,3,4,5,6)
    }
  }

  // 3.21

  "#filterViaFlatMap" should {

    "remove elements that don't satisfy a predicate" in {
      filterViaFlatMap(List(1,2,3,4,5))(_ % 2 == 0)
    }
  }

  // 3.22

  "#zipInts" should {

    "zip two lists of ints by adding them" in {
      zipInts(List(1,2,3), List(3,2,1)) shouldEqual List(4,4,4)
    }
  }

  // 3.23

  "#zipWith" should {

    "zip two arbitary lists with an arbitrary function" in {
      zipWith(List("hi","there"), List("Austin","Guest"))(_+_) shouldEqual List("hiAustin", "thereGuest")
    }
  }
}
