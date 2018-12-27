package structures

/**
  * @author tomer
  * @since 12/25/18
  */
final case class HyperEdge[Node, EdgeType](target: Node, edgeType: EdgeType, sources:Seq[Node], metadata: Metadata)

trait Metadata extends collection.immutable.Iterable[Metadata] {
  def merge(other: Metadata): Metadata = UnionMetadata(other.toSet ++ this.toSet)

  override def iterator: Iterator[Metadata] = Iterator(this)

  protected def toStr: String

  override def toString(): String = toStr
}

case object EmptyMetadata extends Metadata {
  override def toStr: String = "EmptyMetadata"
}

case class UnionMetadata(datas: Set[Metadata]) extends Metadata {
  override def iterator: Iterator[Metadata] = datas.toIterator.flatMap(_.iterator)
  override def toStr: String = iterator mkString ", "
}