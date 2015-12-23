package parser

import scala.collection.immutable.List
import scalaz.MonadPlus
import scala.language.implicitConversions
import scala.language.higherKinds

package object generic {

  type Error = String
  type Result[+A] = Either[Error, A]
  case class State(text: String, index: Int)
  type Parser[+A] = State => (State, Result[A])

  object ParserMonad extends MonadPlus[Parser] { self =>

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    def succeed[A](a: => A): Parser[A] = point(a)
    def point[A](a: => A): Parser[A] = s => (s, Right(a))

    def bind[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
      s => p(s) match {
        case (t, Left(err)) => (t, Left(err))
        case (t, Right(a)) => f(a)(t)
      }

    def fail[A](msg: String): Parser[A] = s => (s, Left(msg))
    def empty[A] = fail("")

    def plus[A](p: Parser[A], q: => Parser[A]): Parser[A] =
      r => p(r) match {
        case (s, Left(errp)) => q(r) match {
          case (t, Left(errq)) => (r, Left(errp))
          case (t, Right(b)) => (t, Right(b))
        }
        case (s, Right(a)) => (s, Right(a))
      }

    def count[A](p: Parser[A]): Parser[Int] = many(p) map (_.length)

    case class ParserOps[A](p: Parser[A]) {
      def >>=[B](f: A => Parser[B]): Parser[B] = self.bind(p)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.bind(p)(f)
      def ||(q: => Parser[A]): Parser[A] = self.plus(p, q)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
    }
  }

}
