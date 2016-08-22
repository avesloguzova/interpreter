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

  val expressions =
    Table(
      "sexpressions",
      Number(100),
      Atom("abc"),
      Atom("->"),
      List(Seq(Number(1))),
      List(Seq.empty)
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

  property("toString . parse = id") {
    forAll(expressions) { expr =>
      Parser.parse(expr.toString).get should be equals expr
    }
  }

  def shouldNotBeEmpty(input: String): Unit = {
    Parser.parse(input) should not be empty
  }
}



