package fpinscala.parsing

import fpinscala.testing._
import Prop._
//import sun.org.mozilla.javascript.NativeJSON
import language.higherKinds
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  // implicit conversions
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String])
  :ParserOps[String] = ParserOps(f(a))

  // combinators
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap(f andThen succeed) // equiv to for { a <- b } yield
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { a <- p; b <- p2 } yield f(a,b) // equiv to p flatMap (a => p2 map (b => f(a,b))
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    for { a <- p; b <- p2 } yield (a,b)

  // abstract parsers
  def succeed[A](a:A): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]

  // concrete parsers
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def defaultSucceed[A](a:A): Parser[A] = string("") map (_ => a)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p,many(p))(_::_) | succeed(List())
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p,many(p))(_::_)
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p,listOfN(n-1,p))(_::_)

  case class ParserOps[A](p: Parser[A]) {

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B,C](p2: => Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p,p2)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def slice: Parser[String] = self.slice(p)
    def many: Parser[List[A]] = self.many(p)
    def succeed(a:A): Parser[A] = self.succeed(a)

    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def **[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def product[B>:A](p2: => Parser[B]): Parser[(A,B)] = self.product(p,p2)
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(str => run(p1)(str) == run(p2)(str))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](in: Gen[(A,String)]): Prop =
      forAll(in){ case(a,str) => run(succeed(a))(str) == Right(a) }

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

object Parsers {

 /* def jsonParser[Parser[+_]](P: Parsers[Parser]):Parser[NativeJSON] = {
    import P._
    val spaces: Parser[String] = char(' ').many.slice

  }*/
}