package parsers

import scala.math.min
import scala.language.implicitConversions

sealed trait OrgNode
object OrgNode {
  type Document = Tree[OrgNode]

  case object Root extends OrgNode
  case class Header(level: Int, content: Line) extends OrgNode
  case class Paragraph(content: List[Line]) extends OrgNode
  case class Blank(content: Line) extends OrgNode

  def descends(parent: OrgNode)(child: Header): Boolean = parent match {
    case Header(level, _) => child.level > level
    case _ => true
  }

  type Line = (String, Marker)

  trait Marker
  case object EOL extends Marker
  case object EOF extends Marker
}

abstract class ParseState {
  def char: Option[Char]
  def consume(count: Int): ParseState
  def position: Int
}

object ParseState {

  case class IndexedString(text: String, pos: Int) extends ParseState {
    def char: Option[Char] = if (pos < text.length) Some(text.charAt(pos)) else None
    def consume(count: Int): ParseState = ParseState(text, min(pos + count, text.length))
    def position: Int = pos
  }

  def apply(text: String, pos: Int): ParseState = IndexedString(text, pos)
}

case class OrgParser[+A](run: ParseState => (ParseState, Result[A])) {
}

object OrgParsers extends Parsers[OrgParser] {

  import OrgNode._

  def doc(root: OrgNode = Root): OrgParser[Document] = {
    val subsection = (header guard descends(root)) >>= (sub => doc(sub))
    val leaf = (blank or paragraph) map (x => Tree(x, List.empty))
    val branch = subsection or leaf
    (branch until eof) map (branches => Tree(root, branches))
  }

  def header: OrgParser[Header] = apply2(level, line)(Header)

  def paragraph: OrgParser[Paragraph] =
    ((line until (header or blank or eof)) guard (ls => !ls.isEmpty)) map Paragraph

  def blank: OrgParser[Blank] = line guard {
    case (str, EOL) => str.trim.isEmpty
    case (str, EOF) => str.trim.isEmpty & !str.isEmpty
  } map Blank

  def level: OrgParser[Int] = (count('*') guard (n => n > 0)) >>= (n => char(' ') >> point(n))

  def line: OrgParser[Line] = {
    val marker = eol or eof
    ((any(_.char) until marker) ** marker) map {case (cs, m) => (cs.mkString, m)}
  }

  def eol: OrgParser[Marker] = char('\n') >> point(EOL)

  def eof: OrgParser[Marker] = OrgParser(before =>
    before.char match {
    case Some(c) => (before, Failure(List((s"expected 'eof' not '$c'", before.position))))
    case None => (before.consume(1), Success(EOF))
  })

  implicit def char(c: Char): OrgParser[Char] = select(_.char, c, 1)

  def any[A](next: ParseState => Option[A], consumes: Int = 1): OrgParser[A] =
    OrgParser(before =>
      next(before) match {
        case Some(a) => (before.consume(consumes), Success(a))
        case None => (before, Failure(List((s"no more input", before.position))))
      })

  def select[A](next: ParseState => Option[A], expected: A, consumes: Int): OrgParser[A] =
    OrgParser(before =>
      next(before) match {
        case Some(a) =>
          if (a == expected) (before.consume(consumes), Success(a))
          else (before, Failure(List((s"expected '$expected' not '$a'", before.position))))
        case None =>
          (before, Failure(List((s"no more input when matching '$expected'", before.position))))
      })

  def run[A](p: OrgParser[A])(input: String) = p.run(ParseState(input, 0))._2

  def invert(p: OrgParser[Unit]): OrgParser[Unit] = OrgParser(before =>
    p.run(before) match {
      case (after, Success(_)) => (before, Failure(List.empty))
      case (after, Failure(_)) => (before, Success(()))
    })

  def attempt[A](p: OrgParser[A]) = OrgParser(before =>
    p.run(before) match {
      case success @ (_, Success(_)) => success
      case (_, failure @ Failure(_)) => (before, failure)
    })

  def bind[A, B](p: OrgParser[A])(f: (A) => OrgParser[B]): OrgParser[B] = OrgParser(before =>
    p.run(before) match {
      case (after, Success(result)) => f(result).run(after)
      case (after, failure @ Failure(_)) => (before, failure)
    })

  def point[A](a: => A): OrgParser[A] = OrgParser(before => (before, Success(a)))

  def plus[A](p: OrgParser[A], q: => OrgParser[A]): OrgParser[A] = OrgParser(before =>
    p.run(before) match {
      case success @ (after, Success(_)) => success
      case (_, Failure(first)) => q.run(before) match {
        case success @ (after, Success(_)) => success
        case (_, Failure(second)) => (before, Failure(first ++ second))
      }
    })

  def empty[A]: OrgParser[A] = OrgParser(before =>
    (before, Failure(List.empty)))
}
