package parsers

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.MonadPlus

abstract class Result[+A]
case class Success[+A](get: A) extends Result[A]
case class Failure(get: List[(String, Int)]) extends Result[Nothing]

trait Parsers[Parser[+_]] extends MonadPlus[Parser] {self =>
  // requires point, empty, bind, plus, invert
  def attempt[A](p: Parser[A]): Parser[A]
  def invert(p: Parser[Unit]): Parser[Unit]
  def run[A](p: Parser[A])(input: String): Result[A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def erase[A, B](a: A)(implicit f: A => Parser[B]): Parser[Unit] =
    f(a) map (b => ())

  def sequence[A](p: Parser[A]): Parser[List[A]] =
    apply2(p, sequence(p))(_::_) or point(List.empty)

  def ignore[A](p: Parser[Unit]): A => Parser[A] =
    a => (p >> point(a))

  def product[A, B](p: Parser[A], q: Parser[B]): Parser[(A, B)] =
    apply2(p, q)((_, _))

  def guard[A](p: Parser[A], pred: A => Boolean) =
    p >>= (a => if (pred(a)) point(a) else empty)

  def until[A, B](p: Parser[A], marker: Parser[B]): Parser[List[A]] =
    sequence(invert(marker) >> p)

  def mkString[A](p: Parser[List[A]]) = p map (a => a.mkString)

  def first[A, B](p: Parser[(A, B)]) = p map {case (a, b) => a}

  def count[A](p: Parser[A]): Parser[Int] =
    apply2(p, count(p))((a, i) => 1 + i) or point(0)

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](q: Parser[B]): Parser[B] = self.plus(p, q)
    def >>[B](q: Parser[B]): Parser[B] = self.bind(p)(_ => q)
    def **[B](q: Parser[B]): Parser[(A, B)] = self.product(p, q)
    def >>=[B](f: (A) => Parser[B]) = self.bind(p)(f)
    def until[B](marker: Parser[B]): Parser[List[A]] = self.until(p, marker)
    def guard(pred: A => Boolean) = self.guard(p, pred)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def bind[B](f: (A) => Parser[B]) = self.bind(p)(f)
  }

}
