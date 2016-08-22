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

  property("Parser result should not return None on correct input") {
    forAll(numbers)(shouldNotBeEmpty)
    forAll(atoms)(shouldNotBeEmpty)
    forAll(lists)(shouldNotBeEmpty)
  }

  def shouldNotBeEmpty(input: String): Unit = {
    Parser.parse(input) should not be empty
  }
}



