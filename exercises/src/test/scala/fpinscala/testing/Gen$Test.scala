package fpinscala.testing

import fpinscala.state.RNG.Simple
import fpinscala.state.{RNG, State}
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by aguestuser on 5/22/16.
  */
class Gen$Test extends WordSpec with Matchers {

  "Gen companion object" should {

    "provide #unit" which {

      "returns a Gen object wraping the value it is passed" in {
        Gen.unit(1).sample.run(Simple(42)) shouldEqual Gen(State.unit(1)).sample.run(Simple(42))
      }
    }

    "provide #listOfN" which {

      "generates a sample List with length N" in {
        Gen.listOfN(3, Gen(State(RNG.int))).sample.run(Simple(1)) shouldEqual
          (List(384748, -1151252339, -549383847),Simple(245470556921330L))
      }
    }

    "provide #union" which {

      "samples from two generators with equal frequency" in {
        Gen.listOfN(6,
          Gen.union(
            Gen(State(RNG.nonNegInt)), Gen(State(RNG.negInt))
          )).sample.run(Simple(2)) shouldEqual
          (
            List(-1988230382, -1827708387, 1559240708, 1733001958, -1735066176, -134740565),
            Simple(8830357631022L)
            )

      }
    }

    "provide #weighted" which {

      "samples from two generators with a probablity proportionate to weights" when {

        "weight of one generator is 0" should {

          "never sample from that generator" in {
            val ints = Gen.listOfN(2,
              Gen.weighted(
                (Gen(State(RNG.nonNegInt)), 1.0),
                (Gen(State(RNG.negInt)),0.0)
            )).sample.run(Simple(1))._1
            all(ints) should be >= 0
          }
        }
      }
    }
  }
}

class Prop$Test extends WordSpec with Matchers {

  import Prop._
  import Gen._

  lazy val intList = listOfN(5, choose(0,100))
  lazy val varIntList = listOf(choose(0,100))


  "#run" when {

    "testing properties of List#reverse" when {

      "property holds" should {

        "return string reporting passed props" in {

          val reverseProp = forAll(varIntList)(ns => ns.reverse.reverse == ns) &&
            forAll(varIntList)(ns => ns.headOption == ns.reverse.lastOption)

          Prop.run(reverseProp) shouldEqual "+ OK, passed 100 tests."
        }
      }

      "property is falsifiable" should {

        "return string reporting failure" in {
          val failingProp = forAll(varIntList)(ns => ns.reverse == ns)
          Prop.run(failingProp, rng = Simple(1)) shouldEqual
            "! Falsified after 0 passed tests:\n List(48, 38)"
        }
      }
    }

    "testing properties of List#max" in {
      // requires implementing `ListOf1` to avoid emptyList
      pending
    }

    "testing properties of Par#map" should {

      "verify that map(unit(1)(_+1) == unit(2)" in {
        // requires implementing check(p: => Boolean): Prop
        pending
      }

      "verify that map(unit(x))(f) == unit(f(x)) for finite set of x" in {
        // requires implementing #pint
        pending
      }
    }

    "testing properties of Stream#take" in { pending }
    "testing properties of Stream#unfold" in { pending }
    "testing properties of Tree#fold" in { pending }
    "testing properties of Option#sequence" in { pending }
    "testing properties of Either#sequence" in { pending }
  }

  "#forAll" should {

    "compose a property from generated input and a boolean function" when {

      "properties are true" should {

        "return Passed" in {
          forAll(intList)(ns => ns.reverse.reverse == ns)
            .run(5,5,Simple(1)) shouldEqual
            Passed
        }
      }

      "properites are false" should {

        "return Falisified" in {
          forAll(intList)(ns => ns.reverse.reverse != ns)
            .run(5,5,Simple(1)) shouldEqual
            Falsified("List(48, 38, 46, 41, 41)", 0)
        }
      }
    }
  }

  "#&&" should {

    "combine two properties using a boolean AND" when {

      "both properties are true" should {

        "return Passed" in {
          forAll(intList)(ns => ns.reverse.reverse == ns) &&
            forAll(intList)(ns => ns.headOption == ns.reverse.lastOption) run(5,5,Simple(1)) shouldEqual
            Passed
        }
      }

      "one property is false" should {

        "return Falsified" in {
          forAll(intList)(ns => ns.reverse.reverse == ns) &&
            forAll(intList)(ns => ns.headOption != ns.reverse.lastOption) run(5,5,Simple(1)) shouldEqual
            Falsified("List(48, 38, 46, 41, 41)", 0)
        }
      }
    }
  }

  "#||" should {

    "combine two properties using a boolean OR" when {

      "both properties are true" should {

        "return Passed" in {
          forAll(intList)(ns => ns.reverse.reverse == ns) ||
            forAll(intList)(ns => ns.headOption == ns.reverse.lastOption) run(5,5,Simple(1)) shouldEqual
            Passed
        }
      }

      "one property is false" should {

        "return Passed" in {
          forAll(intList)(ns => ns.reverse.reverse != ns) ||
            forAll(intList)(ns => ns.headOption == ns.reverse.lastOption) run(5,5,Simple(1)) shouldEqual
            Passed
        }
      }

      "both properties are false" should {

        "return Falisfied with traces for both failures" in {
          forAll(intList)(ns => ns.reverse.reverse != ns) ||
            forAll(intList)(ns => ns.headOption != ns.reverse.lastOption) run(5,5,Simple(1)) shouldEqual
            Falsified("List(48, 38, 46, 41, 41)\nList(48, 38, 46, 41, 41)", 0)
        }
      }
    }
  }
}
