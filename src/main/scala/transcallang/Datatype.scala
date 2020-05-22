package transcallang

/** An algebric datatype definition.
  *
  * @param name of datatype.
  * @param constructors tuple of constructor name and type
  */
case class Datatype(name: Identifier, typeParameters: Seq[AnnotatedTree], constructors: Seq[Identifier]) {
  // TODO: Return this once TheSy action reveives datatypes
//  assert(constructors.forall(c => {
//    val constructorReturnType = if (c.annotation.get.root == Language.mapTypeId) c.annotation.get.subtrees.last else c.annotation.get
//    c.annotation.nonEmpty && constructorReturnType == asType
//  }))

  def asType: AnnotatedTree = AnnotatedTree.withoutAnnotations(name, typeParameters)
}
