package transcallang

trait Namespace {

}

case class Identifier(literal: String, annotation: Option[AnnotatedTree]=None, namespace: Option[Namespace]=None) {
  override def equals(obj: Any): Boolean = obj match {
    case Identifier(l, a ,n) => l == literal && (a == annotation || annotation.isEmpty || a.isEmpty) && n == namespace
    case _ => false
  }

  def cleanTypes: Identifier = copy(annotation = None)

  def isReference: Boolean = literal.startsWith("?")

  def cleanReferences: Identifier = if(isReference) copy(literal = literal.drop(1)) else this

  def isFunction: Boolean = annotation.get.root == Language.mapTypeId

  override def toString: String = "Identifier(\"" + s"$literal" + "\", " + s"$annotation, $namespace)"
}

object Identifier {
  def apply(literal: String): Identifier = new Identifier(literal, None, None)
}

case class AnnotatedTree(root: Identifier, subtrees: Seq[AnnotatedTree], annotations: Seq[AnnotatedTree]=Seq()) {
  def getType: Option[AnnotatedTree] = root.annotation
  def getRetType: Option[AnnotatedTree] = root.annotation.map(AnnotatedTree.extractRetType)

  def cleanTypes: AnnotatedTree = this.map(_.cleanTypes)

  def isLeaf: Boolean = subtrees.isEmpty

  def nodes: Stream[AnnotatedTree] = this #:: {subtrees.toStream flatMap (x => x.nodes)}

  def leaves: Stream[AnnotatedTree] = nodes filter (_.isLeaf)
  def terminals: Stream[Identifier] = leaves map (_.root)

  def size: Int = 1 + (subtrees map (_.size)).sum
  lazy val depth: Int = if (subtrees.isEmpty) 0 else subtrees.map(_.depth).max + 1

  def map[S](rootOp: Identifier => Identifier) : AnnotatedTree =
    copy(root=rootOp(root), subtrees=subtrees.map (_.map(rootOp)))

  def hasDescendant(descendant: AnnotatedTree): Boolean = nodes contains descendant

  def replaceDescendant(switch: (AnnotatedTree, AnnotatedTree)): AnnotatedTree =
    if (switch._1 == this) switch._2
    else copy(subtrees=subtrees map (_.replaceDescendant(switch)))

  def replaceDescendants(switch: Seq[(AnnotatedTree, AnnotatedTree)]): AnnotatedTree =
    switch find (_._1 == this) match {
      case Some(sw) => sw._2
      case _ => copy(subtrees=subtrees map (_.replaceDescendants(switch)))
    }

  def split(symbol: Identifier): Seq[AnnotatedTree] =
    if (root == symbol) subtrees.flatMap(_.split(symbol))
    else Seq(this)
}

object AnnotatedTree {
  def identifierOnly(root: Identifier): AnnotatedTree = new AnnotatedTree(root, Seq.empty, Seq.empty)
  def withoutAnnotations(root: Identifier, subtrees: Seq[AnnotatedTree]): AnnotatedTree = new AnnotatedTree(root, subtrees, Seq.empty)

  def extractRetType(ty: AnnotatedTree): AnnotatedTree =
    if (ty.root == Language.mapTypeId) extractRetType(ty.subtrees.last)
    else ty
}
