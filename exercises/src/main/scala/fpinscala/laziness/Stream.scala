package fpinscala.laziness

import Stream._
import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this.foldRight(List[A]())(_ :: _)

  def toListSafe: List[A] = {
    @tailrec
    def go(as: Stream[A], acc: List[A]): List[A] = as match {
      case Empty => acc
      case Cons(h,t) => go(t(), h() :: acc)
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h,t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this, n){
      case (Cons(h,t), 1) => Some(h(), (empty,0))
      case (Cons(h,t), _) if n > 1 => Some(h(), (t(), n-1))
      case _ => None
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h,t) if p(h()) => Some(h(),t())
      case _ => None
    }

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) =>if (p(a)) cons(a,b) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()),t())
      case _ => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def zipWith[B,C](s: => Stream[B])(f: (A,B) => C): Stream[C] =
    unfold(this, s){
      case (Cons(h,t), Cons(h1,t1)) => Some(f(h(),h1()), (t(),t1()))
      case _ => None
    }

  def zip[B](s: => Stream[B]): Stream[(A,B)] = zipWith(s)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s)
      .takeWhile{ case(_,r) => r.isDefined }
      .forAll{ case(l,r) => l == r }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    } append Stream(empty)

  def hasSubsequence[A](s: => Stream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, =>B) => B): Stream[B] =
    foldRight(z, Stream(z))((a, p) => {
      lazy val p0 = p
      val b = f(a, p0._1)
      (b, cons(b, p0._2))
    })._2

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a+b))
    go(0,1)
  }

  def unfold_1[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a:A, s:S)) => cons(a, unfold(s)(f))
      case _ => empty
    }

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A]){ case(a,s) => cons(a, unfold(s)(f)) }

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1,1))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(m => Some(m, m +1))
  val fibsViaUnfold: Stream[Int] = unfold(0,1) { case(f0,f1) => Some(f0, (f1, f0+f1)) }

}