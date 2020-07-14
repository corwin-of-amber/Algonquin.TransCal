package synthesis.search.actions.thesy

import transcallang.{AnnotatedTree, Datatype}

/** A class representing the concepts of a theory T. Only symbol names and types, i.e. concepts, are given here.
  * The semantics, or rather facts, are given seperatly as rewrite rules
  *
  * @param datatypes
  * @param definitions function symbols
  */
case class SortedVocabulary(datatypes: Seq[Datatype], definitions: Seq[AnnotatedTree]) {
  assert(definitions.forall(_.getType.nonEmpty))
  assert(datatypes.length + definitions.length > 0)

  def allSymbols: Seq[AnnotatedTree] = datatypes.flatMap(_.constructors
    .map(c => AnnotatedTree.identifierOnly(c))) ++ definitions
}
