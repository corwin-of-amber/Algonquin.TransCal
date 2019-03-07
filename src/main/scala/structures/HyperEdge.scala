package structures

import scala.collection.mutable

/**
  * @author tomer
  * @since 12/25/18
  */
final case class HyperEdge[+Node, +EdgeType](target: Node, edgeType: EdgeType, sources:Seq[Node], metadata: Metadata) {
  private val tup = (target, edgeType, sources)

  override def hashCode(): Int = tup.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case rule: HyperEdge[Node, EdgeType] => tup.equals(rule.tup)
    case _ => false
  }
}

trait Metadata extends collection.immutable.Iterable[Metadata] {
  def merge(other: Metadata): Metadata = UnionMetadata(other.toSet ++ this.toSet)

  override def iterator: Iterator[Metadata] = Iterator(this)

  protected def toStr: String

  override def toString(): String = toStr
}

trait SpecificMergeMetadata extends Metadata {
  def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata
}

case object EmptyMetadata extends Metadata {
  override def toStr: String = "EmptyMetadata"
}

final class UnionMetadata private (datas: Set[Metadata]) extends Metadata {
  override def iterator: Iterator[Metadata] = datas.toIterator.flatMap(_.iterator)
  override def toStr: String = iterator mkString ", "
}
object UnionMetadata {
  def apply(datas: Set[Metadata]): UnionMetadata = {
    val noSpecific = datas.filterNot(_.isInstanceOf[SpecificMergeMetadata])
    val specific = datas.filter(_.isInstanceOf[SpecificMergeMetadata]).map(_.asInstanceOf[SpecificMergeMetadata])
    val specificMerged = specific.foldLeft(mutable.HashMultiMap.empty[Class[_], SpecificMergeMetadata])((m, data)=>m.addBinding(data.getClass, data))
      .values.map(_ reduce (_ mergeSpecific _))
    new UnionMetadata(noSpecific ++ specificMerged)
  }

}
