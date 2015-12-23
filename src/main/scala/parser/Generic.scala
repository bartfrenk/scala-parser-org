package parser

import scala.collection.immutable.List
import scalaz.MonadPlus
import scala.language.implicitConversions
import scala.language.higherKinds

object Parser {

  type Error = String
  type Result[+A] = Either[Error, A]

  def succeed[S, A](a : => A): Parser[S, A] = Parser(s => (s, Right(a)))
  def fail[S, A](err: Error): Parser[S, A] = Parser(s => (s, Left(err)))

  implicit def parserMonad[S] = new MonadPlus[({type f[x] = Parser[S, x]})#f] { self =>

    def point[A](a: => A): Parser[S, A] = Parser.succeed(a)
    def bind[A, B](p: Parser[S, A])(f: A => Parser[S, B]): Parser[S, B] = p flatMap f
    def empty[A] = Parser.fail("")
    def plus[A](p: Parser[S, A], q: => Parser[S, A]): Parser[S, A] = p or q

    def count[A](p: Parser[S, A]): Parser[S, Int] = many(p) map (_.length)
  }

}

case class Parser[S, +A](run: S => (S, Parser.Result[A])) {

  def flatMap[B](f: A => Parser[S, B]): Parser[S, B] =
    Parser(s => run(s) match {
      case (t, Left(err)) => (t, Left(err))
      case (t, Right(a)) => f(a).run(t)
    })

  def map[B](f: A => B): Parser[S, B] = {
    val g: A => Parser[S, B] = f andThen (b => Parser(s => (s, Right(b))))
    flatMap(g)
  }

  def or[B >: A](p: => Parser[S, B]): Parser[S, B] =
    Parser(r => run(r) match {
      case (s, Left(errp)) => p.run(r) match {
        case (t, Left(errq)) => (r, Left(errp))
        case (t, Right(b)) => (t, Right(b))
      }
      case (s, Right(a)) => (s, Right(a))
    })

}
