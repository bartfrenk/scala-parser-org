package parser

import scala.language.implicitConversions

object Org {

  abstract class Node
  case class Header(level: Int, text: String)
  case class Plaintext(text: String)

  //type Document = Tree[Node]

  abstract class ParseState {
    def next: Option[Char]
    def head(count: Int): Option[String]
    def advance(count: Int = 1): ParseState
    def toString: String
  }

  object ParseState {
    def parse[A](p: Parser[ParseState, A], text: String) = p.run(create(text, 0))
    def create(text: String, index: Int): ParseState = new ParseState {
      override def next: Option[Char] =
        if (index < text.length) Some(text.charAt(index)) else None
      override def head(count: Int): Option[String] =
        if (index + count <= text.length) Some(text.substring(index, index + count))
        else None
      override def advance(count: Int = 1): ParseState = create(text, index + count)
      override def toString: String = text + ": " + index
    }
  }


  object Combinators extends Parser.Combinators[ParseState] {
    implicit def char(c: Char): Parser[ParseState, Char] = Parser(s =>
      s.next match {
        case Some(d) =>
          if (c == d) (s.advance(), Right(c))
          else (s, Left("expected " + c))
        case None => (s, Left("end of input"))
      })

    implicit def string(exp: String): Parser[ParseState, String] = Parser(s =>
      s.head(exp.length) match {
        case Some(act) =>
          if (act.equals(exp)) (s.advance(exp.length), Right(exp))
          else (s, Left("expected '" + exp + "'"))
        case None => (s, Left("end of input"))
      })

    def except(c: Char): Parser[ParseState, Char] = Parser(s =>
      s.next match {
        case Some(d) =>
          if (c == d) (s, Left("did not expect " + c))
          else (s.advance(), Right(d))
        case None => (s, Left("end of input"))
      })

    def until(c: Char): Parser[ParseState, String] =
      (many(except(c)) >>= skip(c)) map (_.mkString)

    def line: Parser[ParseState, String] = until('\n')

    /*
    def tree(root: Node): Document = {
      def descendsFrom(node: Node): Bool = (root, node) match {
        case (Header(rl, _), Plaintext(_)) => true
        case (Header(rl, _), Header(nl, _)) => rl < nl
        case _ => false
      }
      val children: List[Tree[Node]] = take(header or plaintext, tree, descendsFrom)
    }
     */

    def header: Parser[ParseState, Header] =
      count('*') >>= skip(' ') >>= (level =>
        if (level < 1) fail("expected header")
        else until('\n') >>= (text => succeed(Header(level, text))))

    /*
    def plaintext: Parser[ParseState, Plaintext] = take(line, noHeader)
     */

    def guard[A](p: A => Boolean): A => Parser[ParseState, A] =
      (a => if (p(a)) succeed(a) else empty)

    def tree[A](node: Parser[ParseState, A], descends: (A, A) => Boolean): A => Parser[ParseState, Tree[A]] =
      root => {
        val branch = node >>= guard(a => descends(root, a)) >>= tree(node, descends)
        many(branch) map (children => Tree(root, children))
      }

    def descends(parent: Header, child: Header) = parent.level < child.level

    def orgp = tree(header, descends)(Header(0, ""))

    def check[A](p: Parser[ParseState, A], pred: A => Boolean): Parser[ParseState, A] =
      p >>= guard(pred)
    // of course List could be an arbitrary monoid
    //def take(p: Parser[ParseState, A], f: A => Parser[ParseState, B], pred: A => Boolean): Parser[ParseState, List[A]] =

  }


}
