package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }


  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(fn: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(fn(n))
    case Branch(l,r) => Branch(map(l)(fn), map(r)(fn))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(n) => f(n)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)
  def maxViaFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)
  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((dl, dr) => 1 + (dl max dr))

}