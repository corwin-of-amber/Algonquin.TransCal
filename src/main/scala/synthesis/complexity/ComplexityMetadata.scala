package synthesis.complexity

import structures.Metadata

/**
  * @author tomer
  * @since 2/11/19
  */
case class ComplexityMetadata(complexity: Complexity) extends Metadata {
  override protected def toStr: String = f"ComplexityMetadata($complexity)"
}