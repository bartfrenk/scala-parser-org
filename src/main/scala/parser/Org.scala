package parser

object Org {

  abstract class Node
  case class Header(level: Int, text: String)
  case class Plaintext(text: String)

  type Document = Tree[Node]

  import Parser._

  def header[S]: Parser[S, Header] = fail("not implemented")
  def plaintext[S]: Parser[S, Plaintext] = fail("not implemented")

}
