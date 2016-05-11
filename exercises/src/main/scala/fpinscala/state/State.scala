package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i<0) -(i+1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    ( i / (Int.MaxValue.toDouble + 1), r)
  }


  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d),r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r1) = intDouble(rng)
    ((d,i),r1)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List(),rng)
    else {
      val (x,r1) = rng.nextInt
      val (xs,r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r1 => {
      val (a, r2) = ra(r1)
      val (b, r3) = rb(r2)
      (f(a,b),r3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))

  val randIntDouble: Rand[(Int,Double)] = both(int, double)
  val randDoubleInt: Rand[(Double,Int)] = both(double,int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((fn,acc) => map2(fn,acc)(_::_))

  def _ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    r1 => {
      val (a, r2) = f(r1)
      g(a)(r2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def _map[A,B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
    //^-- equiv to: flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))

}

case class State[S,+A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s1 => {
      val (a,s2) = run(s1)
      f(a).run(s2)
    })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a:A): State[S,A] = State((a,_))
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit[S,List[A]](List()))((f, acc) =>f.map2(acc)(_::_))

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  import State._

  def update(i: Input)(s: Machine): Machine = (i,s) match {
      case (_, Machine(_,0,_)) => s
      case(Coin, Machine(false,_,_)) => s
      case(Coin, Machine(true,cndy,coin)) => Machine(false,cndy,coin+1)
      case(Turn, Machine(true,_,_)) => s
      case(Turn, Machine(false,cndy,coin)) => Machine(true,cndy-1,coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
     _ <- sequence(inputs.map(modify[Machine] _ compose update))
     s <- get
    } yield (s.candies, s.coins)
}
