import org.scalatest._
import org.scalatest.prop._

class ParserSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {

  val numbers =
    Table(
      "numbers",
      "1",
      "0",
      "10",
      " 10",
      " 10 "
    )

  val atoms =
    Table(
      "atoms",
      "a",
      "abc",
      "->",
      " abc",
      " abc "
    )
  val lists =
    Table(
      "lists",
      "()",
      "(1)",
      "(1 2 ->)",
      "(    )",
      "((   ))",
      "( 10    )"
    )

  val wrongInputs =
    Table(
      "errors",
      "(",
      ")",
      "a b",
      "92 (",
      "()()"
    )

  property("Parser result should not return None on correct input") {
    forAll(numbers)(shouldNotBeEmpty)
    forAll(atoms)(shouldNotBeEmpty)
    forAll(lists)(shouldNotBeEmpty)
  }

  property("Result should be empty on wrong input") {
    forAll(wrongInputs) { input =>
      Parser.parse(input) shouldBe empty
    }
  }

  def shouldNotBeEmpty(input: String): Unit = {
    Parser.parse(input) should not be empty
  }
}



