package relentless.rewriting

import scala.collection.IndexedSeq

/**
  * @author tomer
  * @since 3/25/18
  */
case class HyperEdge[T](edgeType: T, target: T, params: Seq[T]) extends IndexedSeq[T] {
  def isFinal: Boolean = params.isEmpty

  override def toString: String = s"type: $edgeType target: $target params: ${params mkString " "}"

  override def length: Int = params.length + 2

  def apply(idx: Int): T = if (idx == 0) edgeType else if (idx == 1) target else params(idx-2)
}

case object HyperEdge {
  def apply[T](seq: Seq[T]): HyperEdge[T] = HyperEdge[T](seq(0), seq(1), seq drop 2)
}
