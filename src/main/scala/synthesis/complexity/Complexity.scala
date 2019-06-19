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
  implicit class ConstantComplexityOp(complexity: ConstantComplexity) {
    def ^(that: ConstantComplexity): ConstantComplexity = ConstantComplexity(math.pow(complexity.constant, that.constant).toInt)
    def +(that: ConstantComplexity): ConstantComplexity = ConstantComplexity(complexity.constant + that.constant)
    def *(that: ConstantComplexity): ConstantComplexity = ConstantComplexity(complexity.constant * that.constant)
  }
}
case class ConstantComplexity(constant: Int) extends Complexity {
  override def toString: String = constant.toString
}
case class ContainerComplexity(contained: Any) extends Complexity {
  override def toString: String = contained.toString
}
class LogComplexity private (val inner: Complexity) extends Complexity {
  override def toString: String = f"log($inner)"

  override def equals(obj: Any): Boolean = obj match {
    case LogComplexity(other) => inner.equals(other)
    case _ => super.equals(obj)
  }
}
object LogComplexity {
  def apply(inner: Complexity): Complexity = inner match {
    case PolynomialComplexity(_, exponent) => exponent
    case _ => new LogComplexity(inner)
  }

  def unapply(arg: LogComplexity): Option[Complexity] = Some(arg.inner)
}
case class PolynomialComplexity private (base: Complexity, exponent: Complexity) extends Complexity
class AddComplexity private (complexitiesBasic: Seq[Complexity]) extends Complexity {
  val complexities: Seq[Complexity] = complexitiesBasic.flatMap({ case AddComplexity(inner) => inner ; case x => Some(x)})
  override def toString: String = complexities.mkString(" + ")

  override def equals(obj: Any): Boolean = obj match {
    case AddComplexity(others) => complexities.equals(others)
    case _ => super.equals(obj)
  }
}
object AddComplexity {
  def apply(complexities: Seq[Complexity]): AddComplexity = new AddComplexity(complexities)

  def unapply(arg: AddComplexity): Option[Seq[Complexity]] = Some(arg.complexities)
}
class MultipleComplexity private (val complexities: Seq[Complexity]) extends Complexity {
  override def toString: String = complexities.mkString(" * ")

  override def equals(obj: Any): Boolean = obj match {
    case MultipleComplexity(others) => complexities.equals(others)
    case _ => super.equals(obj)
  }
}
object MultipleComplexity {
  def apply(complexities: Seq[Complexity]): MultipleComplexity = new MultipleComplexity(complexities.flatMap({ case MultipleComplexity(inner) => inner ; case x => Some(x)}))

  def unapply(arg: MultipleComplexity): Option[Seq[Complexity]] = Some(arg.complexities)
}
