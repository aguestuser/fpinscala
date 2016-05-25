package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}



case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  def &&(p:Prop): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Passed => p.run(max,n,rng)
      case f => f
    }
  }

  def ||(p:Prop): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(msg,_) => p.tag(msg).run(max,n,rng)
      case p => p
    }
  }

  def tag(msg: String): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(msg_, sc) => Falsified(msg + "\n" + msg_, sc)
      case p => p
    }
  }

}

object Prop {

  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
  type MaxSize = Int

  lazy val sysTimeRng = RNG.Simple(System.currentTimeMillis)

  // handle 2-arity prop-functions (ignore max param)
  def apply(f: (TestCases,RNG) => Result): Prop = Prop { (_,n,rng) => f(n,rng) }

  // for fixed-size generators
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(gen)(rng).zip(Stream.from(0)).take(n).map{
      case (a, i) =>
        try { if (f(a)) Passed else Falsified(a.toString, i) }
        catch { case e: Exception => Falsified(buildMsg(a,e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  // for variably-sized generators
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop = props.map(p => Prop {
        (max,n,rng) => p.run(max,casesPerSize,rng)
      }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  // convenience method for calling run w/ default values & printing results
  def run(p: Prop, max: Int = 100, numTests: Int = 100, rng: RNG = sysTimeRng): String = {
    val res = p.run(max,numTests,rng) match {
      case Passed => s"+ OK, passed $numTests tests."
      case Falsified(msg,n) => s"! Falsified after $n passed tests:\n $msg"
    }
    println(res)
    res
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
       |generated an exception: ${e.getMessage}
       |stack trace:
       |${e.getStackTrace}
     """.stripMargin

  private val execGen = weighted(
      choose(1,4).map(Executors.newFixedThreadPool) -> .75, // .75 probability of generating fixed size threadpool w/ between 1 & 4 threads
      unit(Executors.newCachedThreadPool) -> .25 // .25 probability of generating unbounded threadpool
    )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(execGen ** g){ case s ** a => f(a)(s).get }

}

case class Gen[+A](sample: State[RNG, A]){

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)

  def unsized: SGen[A] = SGen(_ => this)
}

object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}


object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  val boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choose(start: Int, stop: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stop - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(x => if(x) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / g2._2.abs
    Gen(State(RNG.double)).flatMap(n => if (n < g1Threshold) g1._1 else g2._1)
  }

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))
}


case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
}

