package fpinscala.state

import org.scalatest.{Matchers, WordSpec}
import RNG._


class StateSpec extends WordSpec  with Matchers  {

  "RNG companion object" should {

    "provide #nextInt" which {

      "returns a pseudo-random integer and the next ring" in {
        Simple(42).nextInt shouldEqual (16159453, Simple(1059025964525L))
      }
    }

    // 6.1

    "provide #nonNegativeInt" which {

      "returns a non-negative random interger and the next ring" in {
        Simple(-1).nextInt shouldEqual (-384749,Simple(281449761806750L))
        nonNegativeInt(Simple(-1)) shouldEqual (384748,Simple(281449761806750L))
      }
    }

    // 6.2

    "provide #double" which {

      "returns a double between 0 and 1 and the nextring" in {
        double(Simple(42)) shouldEqual (0.007524831686168909, Simple(1059025964525L))
      }
    }

    // 6.3

    "provide #intDouble" which {

      "returns an Integer-Double pair and the next ring" in {
        intDouble(Simple(42)) shouldEqual ((16159453,0.5967354848980904),Simple(197491923327988L))
      }
    }

    "provide #doubleInt" which {

      "returns a Double-Integer pair and the next ring" in {
        doubleInt(Simple(42)) shouldEqual ((0.5967354848980904,16159453),Simple(197491923327988L))
      }
    }

    "provide #double3" which {

      "returns a 3-tuple of doubles and the next ring" in {
        double3(Simple(42)) shouldEqual (
          (0.007524831686168909, 0.5967354848980904, 0.15846728393808007),
          Simple(259172689157871L)
          )
      }
    }

    // 6.4

    "provide #ints" which {

      "returns a list of N pseudo-random ints and the next ring" in {
        ints(3)(Simple(42)) shouldEqual (List(16159453, -1281479697, -340305902),Simple(259172689157871L))
      }
    }

    // extra

    "provide #boolean" which {
      "returns a boolean and the next ring" in {
        boolean(Simple(42)) shouldEqual (false, Simple(1059025964525L))
        boolean(Simple(1)) shouldEqual (true, Simple(25214903928L))
      }
    }

    // 6.5

    "provide #doubleViaMap" which {

      "returns a double and the next ring" in {
        doubleViaMap(Simple(42)) shouldEqual (0.007524831686168909, Simple(1059025964525L))
      }
    }

    // 6.6

    "provide #randIntDouble via `map2`" which {

      "returns an Integer-Double pair and the next ring" in {
        randIntDouble(Simple(42)) shouldEqual ((16159453,0.5967354848980904),Simple(197491923327988L))
      }
    }

    "provide #randDoubleInt via `map2`" which {

      "returns a Double-Integer pair and the next ring" in {
        doubleInt(Simple(42)) shouldEqual ((0.5967354848980904,16159453),Simple(197491923327988L))
      }
    }

    // 6.7

    "provide #_ints via `sequence`" which {

      "returns a list of pseudo-random intsa and the next ring" in {
        _ints(3)(Simple(42)) shouldEqual (List(16159453, -1281479697, -340305902),Simple(259172689157871L))
      }
    }

    // 6.8

    "provide #nonNegativeLessThan (via `flatMap`)" which {

      "returns a non-negative pseudo-random number less than n and the next ring" in {
        nonNegativeLessThan(Int.MaxValue/2)(Simple(42))._1 shouldBe < (Int.MaxValue/2)
      }
    }
  }

  "Candy" when {

    import State._
    import Candy._

    "#update" when {

      "inserting coin" when {

        "machine is locked and has candy" should {
          "unlock machine" in {
            update(Coin)(Machine(true,1,1)) shouldEqual Machine(false,1,2)
          }
        }
        "machine is unlocked" should {
          "do nothing" in {
            update(Coin)(Machine(false,1,1)) shouldEqual Machine(false,1,1)
          }
        }
        "machine has no candy" should {
          "do nothing" in {
            update(Coin)(Machine(true,0,1)) shouldEqual Machine(true,0,1)
          }
        }
      }
      "turning knob" when {

        "machine is locked" should {
          "do nothing" in {
            update(Turn)(Machine(true,1,1)) shouldEqual Machine(true,1,1)
          }
        }
        "machine is unlocked and has candy" should {
          "dispense candy and lock machine" in {
            update(Turn)(Machine(false,1,1)) shouldEqual Machine(true,0,1)
          }
        }
        "machine has no candy" should {
          "do nothing" in {
            update(Turn)(Machine(false,0,1)) shouldEqual Machine(false,0,1)
          }
        }
      }
    }

    "#simulateMachine" when {

      "machine has 5 candies and 10 coins" when {

        "buying 4 candies" should {

          "leave the machine with 1 candy and 14 coins" in {
            val machine = Machine(true,5,10)
            val inputs = List(Coin,Turn,Coin,Turn,Coin,Turn,Coin,Turn)
            simulateMachine(inputs).run(machine)._1 shouldEqual (1,14)
          }
        }
      }
    }
  }
}
