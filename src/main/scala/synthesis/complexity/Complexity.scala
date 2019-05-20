package synthesis.complexity

/**
  * @author tomer
  * @since 2/11/19
  */
sealed trait Complexity
object Complexity {
  implicit class ComplexityOp(complexity: Complexity) {
    def log: Complexity = LogComplexity(complexity)
    def ^(that: Complexity): PolynomialComplexity = PolynomialComplexity(complexity, that)
    def +(that: Complexity): AddComplexity = AddComplexity(Seq(complexity, that))
    def *(that: Complexity): MultipleComplexity = MultipleComplexity(Seq(complexity, that))
  }
}
object ConstantComplexity extends Complexity
case class ContainerComplexity(contained: Any) extends Complexity
case class LogComplexity private (inner: Complexity) extends Complexity
object LogComplexity {
  def apply(inner: Complexity): Complexity = inner match {
    case PolynomialComplexity(_, exponent) => exponent
    case _ => new LogComplexity(inner)
  }
}
case class PolynomialComplexity private (base: Complexity, exponent: Complexity) extends Complexity
case class AddComplexity private (complexities: Seq[Complexity]) extends Complexity
object AddComplexity {
  def apply(complexities: Seq[Complexity]): AddComplexity = new AddComplexity(complexities.flatMap({ case AddComplexity(inner) => inner ; case x => Some(x)}))
}
case class MultipleComplexity private (complexities: Seq[Complexity]) extends Complexity
object MultipleComplexity {
  def apply(complexities: Seq[Complexity]): MultipleComplexity = new MultipleComplexity(complexities.flatMap({ case MultipleComplexity(inner) => inner ; case x => Some(x)}))
}
