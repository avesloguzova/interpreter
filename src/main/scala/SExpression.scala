sealed trait SExpression

case class Number(value: Int) extends SExpression {
  override def toString: String = value.toString
}

case class Atom(value: String) extends SExpression {
  override def toString: String = value
}

case class List(value: Seq[SExpression]) extends SExpression {
  override def toString: String = value.mkString("(", ",", ")")
}



