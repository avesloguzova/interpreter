import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Parser {

  def parse(input: String): Option[SExpression] = parseSExpr(input).flatMap {
    case (expr, "") => Some(expr)
    case _ => None
  }

  def parseSExpr(input: String): Option[(SExpression, String)] = {
    val trimmedInput = input.trim
    if (trimmedInput.nonEmpty) {
      parseNumber(trimmedInput) match {
        case Some((number, rest)) => Some(Number(number), rest)
        case _ => parseAtom(trimmedInput) match {
          case Some((atom, rest)) => Some(Atom(atom), rest)
          case _ => parseList(trimmedInput).map { case (list, rest) => (List(list), rest) }
        }
      }
    }
    else {
      None
    }
  }

  def parseNumber(input: String): Option[(Int, String)] = {
    val (prefix, suffix) = input.span(_.isDigit)
    if (prefix.isEmpty) None
    else {
      val number = prefix.foldLeft(0) { case (res, char) => res * 10 + char.asDigit }
      Some(number, suffix)
    }
  }


  def parseAtom(input: String): Option[(String, String)] = {
    val res@(prefix, _) = input.span(isAtomCharacter)
    if (prefix.nonEmpty)
      Some(res)
    else
      None
  }

  def parseList(input: String): Option[(Seq[SExpression], String)] = {
    if (input.head == '(') {
      val expressions = new ArrayBuffer[SExpression]
      @tailrec
      def parseListHelper(input: String): Option[String] = {
        lazy val trimmedInput = input.trim
        parseSExpr(input) match {
          case Some((expr, rest)) if rest.nonEmpty && rest.head == ')' =>
            expressions += expr
            Some("")
          case Some((expr, rest)) if rest.nonEmpty =>
            expressions += expr
            parseListHelper(rest)
          case None if trimmedInput.nonEmpty && trimmedInput.head == ')' => Some(trimmedInput.tail)
          case _ => None
        }
      }
      parseListHelper(input.tail).map((expressions, _))
    } else {
      None
    }
  }

  def isAtomCharacter(char: Char) = !(char == '(' || char == ')' || char.isSpaceChar)

}
