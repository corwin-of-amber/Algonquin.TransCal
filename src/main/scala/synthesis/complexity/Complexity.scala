package synthesis.complexity

/**
  * @author tomer
  * @since 2/11/19
  */
sealed trait Complexity
object Complexity {
  implicit class ComplexityOp(complexity: Complexity) {
    def +(that: Complexity): AddComplexity = AddComplexity(Seq(complexity, that))
    def *(that: Complexity): MultipleComplexity = MultipleComplexity(Seq(complexity, that))
  }
  implicit class ConstantComplexityOp(complexity: ConstantComplexity) {
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
class AddComplexity private (complexitiesBasic: Seq[Complexity]) extends Complexity {
  val complexities: Seq[Complexity] = {
    val flatted = complexitiesBasic.flatMap({ case AddComplexity(inner) => inner ; case x => Some(x)})
    val canCalculate = ConstantComplexity(flatted.collect({ case c: ConstantComplexity => c.constant }).sum)
    val cantCalculate = flatted.filterNot(_.isInstanceOf[ConstantComplexity])
    canCalculate +: cantCalculate
  }
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
class MultipleComplexity private (val complexitiesBasic: Seq[Complexity]) extends Complexity {
  val complexities: Seq[Complexity] = {
    val flatted = complexitiesBasic.flatMap({ case MultipleComplexity(inner) => inner ; case x => Some(x)})
    val canCalculate = ConstantComplexity(flatted.collect({ case c: ConstantComplexity => c.constant }).product)
    val cantCalculate = flatted.filterNot(_.isInstanceOf[ConstantComplexity])
    canCalculate +: cantCalculate
  }
  override def toString: String = complexities.mkString(" * ")

  override def equals(obj: Any): Boolean = obj match {
    case MultipleComplexity(others) => complexities.equals(others)
    case _ => super.equals(obj)
  }
}
object MultipleComplexity {
  def apply(complexities: Seq[Complexity]): MultipleComplexity = new MultipleComplexity(complexities)

  def unapply(arg: MultipleComplexity): Option[Seq[Complexity]] = Some(arg.complexities)
}
