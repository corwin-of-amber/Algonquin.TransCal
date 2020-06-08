package synthesis.search.actions.thesy

import transcallang.{AnnotatedTree, Datatype}

case class SortedVocabulary(datatypes: Seq[Datatype], definitions: Seq[AnnotatedTree]) {
  assert(definitions.forall(_.getType.nonEmpty))
  assert(datatypes.length + definitions.length > 0)

  def allSymbols: Seq[AnnotatedTree] = datatypes.flatMap(_.constructors
    .map(c => AnnotatedTree.identifierOnly(c))) ++ definitions
}
