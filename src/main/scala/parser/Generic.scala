package parser

import scala.collection.immutable.List
import scalaz.MonadPlus

object Parser {
  type Error = String
  type Result[+A] = Either[Error, A]

  def parse[S, A](p: Parser[S, A])(s: S) = p.run(s)

  trait Combinators[S] extends MonadPlus[({type f[x] = Parser[S, x]})#f] {

    def succeed[A](a : => A): Parser[S, A] = Parser(s => (s, Right(a)))
    def fail[A](err: Error): Parser[S, A] = Parser(s => (s, Left(err)))

    def point[A](a: => A): Parser[S, A] = succeed(a)
    def bind[A, B](p: Parser[S, A])(f: A => Parser[S, B]): Parser[S, B] = p flatMap f
    def empty[A] = fail("")
    def plus[A](p: Parser[S, A], q: => Parser[S, A]): Parser[S, A] = p or q

    def count[A](p: => Parser[S, A]): Parser[S, Int] = many(p) map (_.length)
    def map2[A, B, C](p: Parser[S, A], q: => Parser[S, B])(f: (A, B) => C): Parser[S, C] =
      for { a <- p; b <- q } yield f(a, b)
    // this is not the same as the ApplicativePlus definition in scalaz
    override def many[A](p: Parser[S, A]): Parser[S, List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List())

    def skip[A, B](p: => Parser[S, A]): B => Parser[S, B] =
      b => many(p) >>= (_ => succeed(b))

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

  def >>=[B](f: A => Parser[S, B]) = flatMap(f)
}
