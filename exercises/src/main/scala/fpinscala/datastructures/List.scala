package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(hd, tl) => Cons(h, tl)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0 ) l
    else l match {
      case Nil => Nil
      case Cons(_,tl) => drop(tl,n-1)
    }


  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(hd,tl) if f(hd) => dropWhile(tl)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,Nil) => Nil
    case Cons(hd, tl) => Cons(hd, init(tl))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,b) => b + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(hd,tl) => foldLeft(tl, f(z,hd))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,hd) => Cons(hd,acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((acc, hd) => f(hd, acc))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((b,a) => Cons(a,b))

  def add1ViaFoldRight(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a,b) => Cons(a+1,b))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def add1ViaFoldLeft(l: List[Int]): List[Int] =
    reverse(foldLeft(l, List[Int]())((b,a) => Cons(a+1,b)))

  def stringifyViaFoldRight(l: List[Int]): List[String] =
    foldRight(l, List[String]())((a,b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a,b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((a,b) => if(f(a)) Cons(a, b) else b)

  def flatMap[A](as: List[A])(f: A => List[A]): List[A] = concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipInts(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h, tl), Cons(h1, tl1)) => Cons(h+h1, zipInts(tl, tl1))
  }

  def zipWith[A](l: List[A], r: List[A])(f: (A,A) => A): List[A] = (l, r) match {
    case (_,Nil) => Nil
    case (Nil,_) => Nil
    case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh),zipWith(lt, rt)(f))
  }
}
