package fpinscala.datastructures

import org.scalatest.{Matchers, WordSpec}
import Tree._

/**
  * Created by aguestuser on 4/21/16.
  */



class TreeSpec extends WordSpec with Matchers {

  val t1: Tree[Int] = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(4)), Leaf(3)))
  val t2 = Branch(Leaf(1), Branch(Branch(Leaf(2), Branch(Leaf(4), Leaf(5))), Leaf(3)))

  // 3.25
  "#size" should {

    "count the nodes in a tree" in {
      Tree.size(t1) shouldEqual 7
    }
  }

  // 3.26
  "#max" should {
    "find the biggest node in a tree" in {
      maximum(t1) shouldEqual 4
    }
  }

  // 3.27
  "#depth" should {
    "find the maximum path length from tree root to any leaf" in {
      depth(t1) shouldEqual 3
      depth(t2) shouldEqual 4
    }
  }

  // 3.28
  "#map" should {
    "apply a function to every element in a tree" in {
      map(t1)(_ + 1) shouldEqual Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4)))
    }
  }

  // 3.29...

  "#sizeViaFold" should {
    "count the nodes in a tree" in {
      sizeViaFold(t1) shouldEqual 7
      sizeViaFold(t2) shouldEqual 9
    }
  }

  "#maxViaFold" should {
    "find the biggest node in a tree" in {
      maxViaFold(t1) shouldEqual 4
      maxViaFold(t2) shouldEqual 5
    }
  }

  "#depthViaFold" should {
    "find the max path length from root to any leaf" in {
      depthViaFold(t1) shouldEqual 3
      depthViaFold(t2) shouldEqual 4
    }
  }

  // ... 3.29
}
