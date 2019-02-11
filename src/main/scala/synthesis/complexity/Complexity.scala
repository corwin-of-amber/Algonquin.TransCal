package synthesis.complexity

import syntax.AstSugar.Term

/**
  * @author tomer
  * @since 2/11/19
  */
sealed trait Complexity {
  def log: Complexity = LogComplexity(this)
  def +(that: Complexity): AddComplexity = that match {
    case AddComplexity(those) => AddComplexity(this +: those)
    case _ => AddComplexity(Seq(this, that))
  }
  def *(that: Complexity): MultipleComplexity = that match {
    case MultipleComplexity(those) => MultipleComplexity(this +: those)
    case _ => MultipleComplexity(Seq(this, that))
  }
}
object ConstantComplexity extends Complexity
case class ContainerComplexity(contained: Term) extends Complexity
class LogComplexity(val inner: Complexity) extends Complexity
object LogComplexity {
  def apply(inner: Complexity): Complexity = inner match {
    case PolynomialComplexity(_, exponent) => exponent
    case _ => new LogComplexity(inner)
  }
  def unapply(arg: LogComplexity): Option[Complexity] = Some(arg.inner)
}
case class PolynomialComplexity(base: Complexity, exponent: Complexity) extends Complexity {
  override def log: Complexity = exponent
}
case class AddComplexity(complexities: Seq[Complexity]) extends Complexity {
  override def +(that: Complexity): AddComplexity = that match {
    case AddComplexity(those) => AddComplexity(complexities ++ those)
    case _ => AddComplexity(complexities :+ that)
  }
}
case class MultipleComplexity(complexities: Seq[Complexity]) extends Complexity {
  override def log: Complexity = AddComplexity(complexities.map(_.log))
  override def *(that: Complexity): MultipleComplexity = that match {
    case MultipleComplexity(those) => MultipleComplexity(complexities ++ those)
    case _ => MultipleComplexity(complexities :+ that)
  }
}
