package parsers

import org.scalatest._
import prop._
import org.scalacheck.Gen

import scala.language.postfixOps


class OrgFunSuite extends FunSuite with PropertyChecks with Matchers {

  import OrgParsers._
  import OrgNode._

  def runT[A](p: OrgParser[A])(input: String) = OrgParsers.run(p)(input) match {
    case Success(a) => a
    case Failure(_) => throw new Exception()
  }

  test("Parse header") {
    forAll(Gen.choose(1, 10), Gen.alphaStr) {(count: Int, text: String) => {
      val input = "*" * count + " " + text + "\n"
      val result = runT(header)(input)
      result.level should equal (count)
      result.content should equal ((text, OrgNode.EOL))
    }}
  }

  /* Need to adapt to new Line type. */
  /*
  test("Parse paragraph correctly") {
    val strings: List[String] = List(
      "In the practice of computing, where we have so much latitude for",
      "making a mess of it, mathematical elegance is not a dispensable",
      "luxury, but a matter of life and death.")
    val input = strings.reduceLeft(_ + "\n" + _) + "\n"
    val result = parse(paragraph, input)
    result should equal (strings)
  }
   */

}
