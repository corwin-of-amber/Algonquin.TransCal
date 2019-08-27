package synthesis.complexity

/**
  * @author tomer
  * @since 2/11/19
  */
object ComplexityPartialOrdering extends PartialOrdering[Complexity] {
  override def tryCompare(x: Complexity, y: Complexity): Option[Int] = {
    (x, y) match {
      case (ContainerComplexity(containedX), ContainerComplexity(containedY)) => if (containedX == containedY) Some(0) else None
      case (ConstantComplexity(ix), ConstantComplexity(iy)) => Ordering.Int.tryCompare(ix, iy)
      case (ConstantComplexity(_), _) => Some(1)
      case (_, ConstantComplexity(_)) => Some(-1)
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
