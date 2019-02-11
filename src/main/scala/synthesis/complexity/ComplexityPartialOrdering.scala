package synthesis.complexity

/**
  * @author tomer
  * @since 2/11/19
  */
object ComplexityPartialOrdering extends PartialOrdering[Complexity] {
  override def tryCompare(x: Complexity, y: Complexity): Option[Int] = {
    (x, y) match {
      case (ContainerComplexity(containedX), ContainerComplexity(containedY)) => if (containedX == containedY) Some(0) else None
      case (ConstantComplexity, ConstantComplexity) => Some(0)
      case (ConstantComplexity, _) => Some(1)
      case (_, ConstantComplexity) => Some(-1)
      case (LogComplexity(innerX), LogComplexity(innerY)) => tryCompare(innerX, innerY)
      case (PolynomialComplexity(baseX, exponentX), PolynomialComplexity(baseY, exponentY)) =>
        tryCompare(baseX, baseY) match {
          case Some(0) => tryCompare(exponentX, exponentY)
          case _ => None
        }
      case _ => None
    }
  }

  override def lteq(x: Complexity, y: Complexity): Boolean = {
    tryCompare(x, y) match {
      case Some(i) => i <= 0
      case None => false
    }
  }
}
