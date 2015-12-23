package parser

package object org {
  abstract class Node
  case class Header(level: Int, text: String)
  case class Plaintext(text: String)

  type Document = Tree[Node]
}
/*
object OrgParsers extends Parsers[Parsers.Parser]

  abstract class Node
  case class Header(level: Int, text: String)
  case class Plaintext(text: String)

  def bind[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    r => p(r) match {
      case (s, Left(err)) => (s, Left(err))
      case (s, Right(a)) => f(a)(s)
    }

  def succeed[A](a: A): Parser[A] = s => (s, Right(a))


  def fail[A](msg: String): Parser[A] = s => (s, Left(msg))

  // org mode specific parsers
  def header: Parser[Header] = fail("not implemented")
  def plaintext: Parser[Plaintext] = fail("not implemented")

  def or[A, B](p: Parser[A], q: => Parser[B]): Parser[Either[A, B]] =
    r => p(r) match {
      case (s, Left(errp)) => q(r) match {
        case (t, Left(errq)) => (r, Left(errp))
        case (t, Right(b)) => (t, Right(Right(b)))
      }
      case (s, Right(a)) => (s, Right(Left(a)))
    }

  def char(c: Char): Parser[Char] =
    s => if (s.next == c)
      (s.advance(1), Right(c))
      else (s, Left("Expected '" + c + "', found '" + s.next + "'"))

 */
