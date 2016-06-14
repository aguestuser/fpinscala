package fpinscala.monoids

import java.util.concurrent.{Executors, ExecutorService}

import fpinscala.datastructures.{Leaf, Branch}
import fpinscala.parallelism.Nonblocking.Par
import fpinscala.testing.Gen
import fpinscala.testing.Prop
import org.scalatest.concurrent.Futures
import org.scalatest.time.{Seconds, Span, Millis}
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext


/**
  * Created by aguestuser on 6/11/16.
  */

class Monoid$Test extends WordSpec with Matchers {

  import Monoid._

  def shouldHold(p:Prop) = Prop.run(p).shouldEqual("+ OK, passed 100 tests.")

  "String Monoid" should {

    "satisfy monoid laws" in {
      shouldHold(monoidLaws(stringMonoid, Gen.string))
    }
  }

  "List Monoid" should {

    "satisfy monoid laws" in {
      shouldHold(monoidLaws(listMonoid[Int], Gen.listOfN(100, Gen.int)))
    }
  }

  "Integer Addition Monoid" should {

    "satisfy monoid laws" in {
      shouldHold(monoidLaws(intAddition, Gen.int))
    }
  }

  "Integer multiplication Monoid" should {

    "satisfy monoid laws" in {
      shouldHold(monoidLaws(intMultiplication, Gen.int))
    }
  }

  "Boolean And Monoid" should {
    "satisfy monoid laws" in {
      shouldHold(monoidLaws(booleanAnd, Gen.boolean))
    }
  }

  "Boolean Or Monoid" in {
    "satisfy monoid laws" in {
      shouldHold(monoidLaws(booleanOr, Gen.boolean))
    }
  }

  "First Option Monoid" should {

    "satisfy monoid laws" in {
      shouldHold(monoidLaws(firstOptionMonoid[Int], Gen.optionInt))
    }
  }

  "Product Monoid" should {

    "satisfy monoid laws" in {
      shouldHold(monoidLaws(productMonoid(stringMonoid,intAddition), Gen.stringInt))
    }
  }

  "Map Merge Monoid" should {

    "satisfy the monoid laws" in {
      shouldHold(monoidLaws(mapMergeMonoid[String,Int](intAddition), Gen.stringIntMap))
    }

    "merge two maps" in {
      mapMergeMonoid[String,Int](intAddition).op(
        Map("foo" -> 1, "bar" -> 1),
        Map("foo" -> 1, "bar" -> 1, "baz" -> 1)
      ) shouldEqual Map("foo" -> 2, "bar" -> 2, "baz" -> 1)

    }
  }

  "#concatenate" should {

    "fold a list of ints with the intAddition monoid" in {
      concatenate(List(1,2,3), intAddition) shouldEqual 6
    }

    "fold a list of strings with the string monoid" in {
      concatenate(List("foo", "bar", "baz"), stringMonoid) shouldEqual "foobarbaz"
    }
  }

  "#foldMap" should {

    "lift a non-monoidal type into a monoidal type" in {
      foldMap(List('a', 'b', 'c'), stringMonoid)(_.toString) shouldEqual "abc"
    }
  }

  "#bag" should {
    "compute a bag from an indexed sequence" in {
      bag(IndexedSeq("foo", "bar", "foo", "foo", "bar", "baz")) shouldEqual
        Map("foo" -> 3, "bar" -> 2, "baz" -> 1)
    }
  }

}

class WC$Test extends WordSpec with Matchers with Futures {

  import fpinscala.monoids.Monoid.WC
  implicit override val patienceConfig = PatienceConfig(
    timeout = Span(10, Seconds),
    interval = Span(100, Millis))

  "#count" should {

    "count the number of words in a string" in {
      WC.count("the cow jumped over the moon") shouldBe 6
    }
  }

  "#parCount" should {
    "count the number of words in a string in parallel" in {
      val es = Executors.newCachedThreadPool
      Par.run(es)(WC.parCount("the count jumped over the mon")) shouldEqual 6
    }
  }
}

class Foldable$Test extends WordSpec with Matchers {

  import Monoid._

  "ListFoldable" should {

    import ListFoldable._

    "implement all methods of Foldable" in {

      foldMap(List('a', 'b', 'c'))(_.toString)(stringMonoid) shouldEqual "abc"
      foldRight(List(1,2,3))(0)(_ + _) shouldEqual 6
      foldLeft(List(1,2,3))(0)(_ + _) shouldEqual 6
      concatenate(List("foo", "bar", "baz"))(stringMonoid) shouldEqual "foobarbaz"
      toList(List(1,2,3)) shouldEqual List(1,2,3)

    }
  }

  "TreeFoldable" should {

    import TreeFoldable._
    val numTree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

    "implement all methods of Foldable" in {
      foldMap(numTree)(identity)(intAddition) shouldEqual 6
      foldLeft(numTree)(0)(_+_) shouldEqual 6
      foldRight(numTree)(0)(_+_) shouldEqual 6
      concatenate(numTree)(intAddition) shouldEqual 6
      toList(numTree) shouldEqual List(1,2,3)
    }
  }

  "OptionFoldable" should {

    import OptionFoldable._

    "implement all methods of Foldable" in {

      foldMap(Some(1))(identity)(intAddition) shouldEqual 1
      foldMap(None)(identity)(intAddition) shouldEqual 0

      foldLeft(Some(1))(1)(_ + _) shouldEqual 2
      foldLeft[Int,Int](None)(1)(_ + _) shouldEqual 1

      foldRight(Some(1))(1)(_ + _) shouldEqual 2
      foldRight[Int,Int](None)(1)(_ + _) shouldEqual 1

      concatenate(Some(1))(intAddition) shouldEqual 1
      concatenate(None)(intAddition) shouldEqual 0

      toList(Some(1)) shouldEqual List(1)
      toList[Int](None) shouldEqual List[Int]()
    }
  }
}

