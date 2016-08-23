import scala.collection.mutable.ArrayBuffer

object Parser {

  def parse(input: String): Option[SExpression] = parseSExpr(input).flatMap {
    case (expr, "") => Some(expr)
    case _ => None
  }

  private def parseSExpr(input: String): Option[(SExpression, String)] = {
    val trimmedInput = input.trim
    if (trimmedInput.isEmpty) {
      None
    } else {
      parseNumber(trimmedInput) match {
        case Some((number, rest)) => Some(Number(number), rest)
        case _ => parseAtom(trimmedInput) match {
          case Some((atom, rest)) => Some(Atom(atom), rest)
          case _ => parseList(trimmedInput).map { case (list, rest) => (List(list), rest) }
        }
      }
    }
  }

  private def parseNumber(input: String): Option[(Int, String)] = {
    val (prefix, suffix) = input.span(_.isDigit)
    if (prefix.isEmpty) None
    else {
      val number = prefix.foldLeft(0) { case (res, char) => res * 10 + char.asDigit }
      Some(number, suffix)
    }
  }


  private def parseAtom(input: String): Option[(String, String)] = {
    val res@(prefix, _) = input.span(isAtomCharacter)
    if (prefix.nonEmpty)
      Some(res)
    else
      None
  }

  private def parseList(input: String): Option[(Seq[SExpression], String)] = {
    if (input.head == '(') {
      parseMany(parseSExpr, input.tail) match {
        case (list, rest) if rest.nonEmpty && rest.head == ')' =>
          Some(list, rest.tail)
        case _ => None
      }
    } else {
      None
    }
  }

  private def parseMany(parse: (String) => Option[(SExpression, String)], input: String): (Seq[SExpression], String) = {
    var rest = input.trim
    val result = new ArrayBuffer[SExpression]
    while (true) {
      parse(rest) match {
        case Some((x, ii)) => rest = ii.trim; result += x
        case None => return (result, rest)
      }
    }
    (result, rest)
  }

  private def isAtomCharacter(char: Char) = !(char == '(' || char == ')' || char.isSpaceChar)

}
