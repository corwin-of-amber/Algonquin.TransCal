package synthesis.search.actions.thesy

import transcallang.{AnnotatedTree, Datatype}

/** A class representing the concepts of a theory T. Only symbol names and types, i.e. concepts, are given here.
  * The semantics, or rather facts, are given seperatly as rewrite rules
  *
  * @param datatypes
  * @param definitions function symbols
  */
case class SortedVocabulary(datatypes: Set[Datatype], definitions: Set[AnnotatedTree]) {
  assert(definitions.forall(_.getType.nonEmpty))
  assert(datatypes.size + definitions.size > 0)

  def allSymbols: Set[AnnotatedTree] = datatypes.flatMap(_.constructors
    .map(c => AnnotatedTree.identifierOnly(c))) ++ definitions

  def prettyPrint =
    s"""${"-" * 60}
       | Data types: ${datatypes.map(_.name.literal).mkString(" ")}
       | Functions: ${definitions.map(_.root.literal).mkString(" ")}
       |${"-" * 60}""".stripMargin

}
