package fpinscala.monads

import fpinscala.state.State
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by aguestuser on 7/19/17.
  */
class MonadTest extends WordSpec with Matchers {
  import Monad._

  "monad identity laws" should {

    "hold for list monad" in {
      listMonad.flatMap(Nil)(listMonad.unit) shouldEqual Nil
      listMonad.flatMap(List(1,2,3))(listMonad.unit) shouldEqual List(1,2,3)
    }

    "hold for option monad" in {
      optionMonad.flatMap(None)(optionMonad.unit) shouldEqual None
      optionMonad.flatMap(Some(1))(optionMonad.unit) shouldEqual Some(1)
    }
  }

  "monad associativity laws" should {

    "hold for list monad" in {
      val lm = listMonad
      def f(n: Int): List[Int] = List(n)
      def g(n: Int): List[Int] = List(n + 1, n + 1)
      def h(n: Int): List[Int] = List(n * n, n * n, n * n)

      lm.compose(lm.compose(f, g), h)(2) shouldEqual List(9,9,9,9,9,9)
      lm.compose(lm.compose(f, g), h)(2) shouldEqual lm.compose(f, lm.compose(g, h))(2)
    }

  }

  "state monad" should {

    val addOneStateMachine = State[Int,Int](s => (s, s + 1))
    val squareStateMachine = State[Int,Int](s => (s, s * s))

    "implement replicateM" in {

      stateMonad.replicateM(3, addOneStateMachine).run(1) shouldEqual (List(1,2,3),4)
      stateMonad.replicateM(3, squareStateMachine).run(2) shouldEqual (List(2,4,16),256)
    }

    "implement map2" in {
      stateMonad.map2(addOneStateMachine, squareStateMachine)(_.toString + _.toString).run(2) shouldEqual ("23", 9)
    }

    "implement sequence" in {
      stateMonad.sequence(List(addOneStateMachine, squareStateMachine, squareStateMachine, addOneStateMachine)).run(2) shouldEqual
        (List(2, 3, 9, 81), 82)
    }

  }
}
