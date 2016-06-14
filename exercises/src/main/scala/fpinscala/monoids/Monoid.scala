package fpinscala.monoids

import fpinscala.datastructures.{Branch, Leaf, Tree}
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(x: A, y: A): A
  val zero: A
}

object Monoid {

  import fpinscala.testing._
  import Prop._

  // laws

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val gen3 = for {x <- gen; y <- gen; z <- gen} yield (x,y,z)
    forAll(gen3)({ case(x,y,z) => m.op(x, m.op(y,z)) == m.op(m.op(x,y), z) }) &&
    forAll(gen)((a: A) => m.op(a, m.zero) == m.op(m.zero, a))
  }

  // instances

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(x: A, y: A): A = m.op(y,x)
    override val zero: A = m.zero
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int = 0
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override val zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override val zero: Option[A] = None
  }

  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def secondOptionMonoid[A]: Monoid[Option[A]] = dual(optionMonoid[A])

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = f compose g
    override val zero: A => A = (a: A) => a
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(x: Par[A], y: Par[A]): Par[A] = x.map2(y)(m.op)
    override val zero: Par[A] = Par.unit(m.zero)
  }

  def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] =
    new Monoid[(A, B)] {
      override def op(x: (A, B), y: (A, B)): (A, B) = (ma.op(x._1, y._1), mb.op(x._2, y._2))
      override val zero: (A, B) = (ma.zero, mb.zero)
    }

  def mapMergeMonoid[K,V](mv: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K, V]] {
    override def op(x: Map[K, V], y: Map[K, V]): Map[K, V] =
      (zero /: (x.keySet ++ y.keySet))((acc, k) =>
        acc.updated(k, mv.op(x.getOrElse(k, mv.zero), y.getOrElse(k, mv.zero))))
    override val zero: Map[K, V] = Map[K,V]()
  }

  // TODO: how to prove this satisfies monad laws?
  def functionMonoid[A,B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(f: A => B, g: A => B): A => B = a => mb.op(f(a),g(a))
    override val zero: A => B = a => mb.zero
  }

  // combinators

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    (m.zero /: as)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    (m.zero /: as)((b,a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as(0))
    else as.splitAt(as.length / 2) match {
      case (l,r) => m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
    }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f) flatMap { bs =>
      foldMapV(bs, par(m))(b =>Par.lazyUnit(b))
    }

  def bag[A](as: IndexedSeq[A]): Map[A,Int] =
    foldMapV(as, mapMergeMonoid[A,Int](intAddition))((a:A) => Map(a -> 1))


  /*def trimMonoid(s: String): Monoid[String] = ???

  def ordered(ints: IndexedSeq[Int]): Boolean = ???
*/
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  object WC {

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      override val zero: WC = Stub("")
      override def op(a: WC, b: WC): WC = (a,b) match {
        case (Stub(c), Stub(d)) => Stub(c + d)
        case (Stub(c), Part(l,count,r)) => Part(c + l, count, r)
        case (Part(l,count,r), Stub(c)) => Part(l,count,r + c)
        case (Part(l1,count1,r1), Part(l2,count2,r2)) =>
          Part(l1, count1 + ( if((r1+l1).isEmpty) 0 else 1) + count2, r2)
      }
    }

    def count(s: String): Int = format(foldMapV(s.toIndexedSeq, wcMonoid)(countChar))
    def parCount(s: String): Par[Int] = parFoldMap(s.toIndexedSeq, wcMonoid)(countChar) map format

    //helpers

    private def countChar(c: Char): WC = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    private def unstub(s: String): Int = if(s.isEmpty) 0 else 1
    private def format(wc: WC): Int = wc match {
      case Stub(str) => unstub(str)
      case Part(l,count,r) => unstub(l) + count + unstub(r)
    }

  }

  /*


  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = ???

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???*/
}

trait Foldable[F[_]] {
  import Monoid._

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}


object ListFoldable extends Foldable[List] {
  def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]) =
    as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))
}

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](a: Option[A])(f: A => B)(mb: Monoid[B]): B =
    a match {
      case None => mb.zero
      case Some(_a) => f(_a)
    }
}

