sealed trait SExpression

case class Number(value: Int) extends SExpression

case class Atom(value: String) extends SExpression

case class List(value: Seq[SExpression]) extends SExpression


