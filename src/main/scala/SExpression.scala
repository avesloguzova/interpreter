sealed trait SExpression

case class Number(value: Int) extends SExpression {
  override def toString: String = value.toString
}

case class Atom(value: String) extends SExpression {
  override def toString: String = value
}

case class List(value: Seq[SExpression]) extends SExpression {
  override def toString: String = value.mkString("(", " ", ")")
}

object List {
  def empty = new List(Seq.empty)

  def apply(head: SExpression, elements: SExpression*): List = new List(head +: elements)
}

