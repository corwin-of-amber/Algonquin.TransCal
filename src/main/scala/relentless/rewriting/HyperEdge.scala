package relentless.rewriting

/**
  * @author tomer
  * @since 3/25/18
  */
abstract class BaseHyperEdge[T](val edgeType: T, val target: T, val params: Seq[T]) extends IndexedSeq[T] {
  def isFinal: Boolean = params.isEmpty

  override def toString: String = s"type: $edgeType target: $target params: ${params mkString " "}"

  override def length: Int = params.length + 2

  def apply(idx: Int): T = if (idx == 0) edgeType else if (idx == 1) target else params(idx-2)
}

case class HyperEdge[T](override val edgeType: T, override val target: T, override val params: Seq[T]) extends
  BaseHyperEdge[T](edgeType, target, params) {}

case object HyperEdge {
  def apply[T](seq: Seq[T]): HyperEdge[T] = HyperEdge[T](seq.head, seq(1), seq drop 2)
}
