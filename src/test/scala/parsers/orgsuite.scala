package parsers

import org.scalatest._
import prop._
import org.scalacheck.Gen

import scala.language.postfixOps

import OrgParsers._

class OrgFunSuite extends FunSuite with PropertyChecks with Matchers {

  test("Parse header correctly") {
    forAll(Gen.choose(1, 10), Gen.alphaStr) {(count: Int, text: String) => {
      val input = "*" * count + " " + text + "\n"
      val result = run(header)(input)
      result.level should equal (level)
      result.content should equal (text, EOL)
    }}
  }

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
