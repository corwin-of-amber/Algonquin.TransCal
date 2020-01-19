package structures

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

object HyperEdge {
  import play.api.libs.json._
  import synthesis.{HyperTermId, HyperTermIdentifier}
  case class JsonEdge()
  implicit val edgeReads: Format[HyperEdge[HyperTermId, HyperTermIdentifier]] = Json.format[HyperEdge[HyperTermId, HyperTermIdentifier]]

  def toJson(hyperEdge: HyperEdge[HyperTermId, HyperTermIdentifier]): String = {
    Json.toJson(hyperEdge).toString()
  }

  def fromJson(text: String): HyperEdge[HyperTermId, HyperTermIdentifier] = {
    Json.fromJson[HyperEdge[HyperTermId, HyperTermIdentifier]](Json.parse(text)).get
  }
}

trait Metadata extends collection.immutable.Iterable[Metadata] {
  def merge(other: Metadata): Metadata = if (other == EmptyMetadata) this else UnionMetadata(other.toSet ++ this.toSet)

  override def iterator: Iterator[Metadata] = Iterator(this)

  protected def toStr: String

  override def toString(): String = toStr
}

trait SpecificMergeMetadata extends Metadata {
  def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata
}

case object EmptyMetadata extends Metadata {
  override def merge(other: Metadata): Metadata = other

  override def toStr: String = "EmptyMetadata"
}

class Uid {}

case class IdMetadata(uid: Uid) extends Metadata {
  override protected def toStr: String = s"IdMetadata($uid)"
}

final class UnionMetadata private (datas: Set[Metadata]) extends Metadata {
  override def iterator: Iterator[Metadata] = datas.iterator
  override def toStr: String = iterator mkString ", "
}
object UnionMetadata {
  def apply(datas: Set[Metadata]): UnionMetadata = {
    val noSpecific = datas.filterNot(_.isInstanceOf[SpecificMergeMetadata])
    val specific = datas.collect{case a: SpecificMergeMetadata => a}
    val specificMerged = specific.foldLeft(scala.collection.mutable.HashMultiMap.empty[Class[_], SpecificMergeMetadata])((m, data)=>m.addBinding(data.getClass, data))
      .values.map(_ reduce (_ mergeSpecific _))
    new UnionMetadata(noSpecific ++ specificMerged)
  }

}
