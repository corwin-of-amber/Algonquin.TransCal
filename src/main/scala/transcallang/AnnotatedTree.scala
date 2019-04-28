package transcallang

trait Namespace {

}

case class Identifier(literal: String, namespace: Option[Namespace]=None)

case class AnnotatedTree(root: Identifier, subtrees: Seq[AnnotatedTree], annotations: Seq[String]) {
  def isLeaf: Boolean = subtrees.isEmpty

  def nodes: Stream[AnnotatedTree] = this #:: {subtrees.toStream flatMap (x => x.nodes)}

  def leaves: Stream[AnnotatedTree] = nodes filter (_.isLeaf)
  def terminals: Stream[Identifier] = leaves map (_.root)

  def size: Int = 1 + (0 /: (subtrees map (_.size)))(_ + _)

  def map[S](rootOp: Identifier => Identifier, annotationsOp: String => String) : AnnotatedTree =
    AnnotatedTree(rootOp(root), subtrees.map (_.map(rootOp, annotationsOp)), annotations.map(annotationsOp))

  def hasDescendant(descendant: AnnotatedTree): Boolean = nodes contains descendant

  def replaceDescendant(switch: (AnnotatedTree, AnnotatedTree)): AnnotatedTree =
    if (switch._1 == this) switch._2
    else AnnotatedTree(root, subtrees map (_.replaceDescendant(switch)), annotations)

  def replaceDescendants(switch: List[(AnnotatedTree, AnnotatedTree)]): AnnotatedTree =
    switch find (_._1 == this) match {
      case Some(sw) => sw._2
      case _ => AnnotatedTree(root, subtrees map (_.replaceDescendants(switch)), annotations)
    }

  override def toString(): String = {
    if (subtrees.isEmpty) root.toString
    else {
      val children = subtrees mkString ", "
      s"$root{$children}"
    }
  }
}
