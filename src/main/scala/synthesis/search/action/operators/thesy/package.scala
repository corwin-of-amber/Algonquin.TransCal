package synthesis.search.action.operators

import synthesis.Programs
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.search.rewrite.RewriteSearchState
import transcallang.{AnnotatedTree, Identifier}

package object thesy {
  trait EquivalenceClasses[T] {
    def getClasses: Map[T, Set[T]]

    def getRepresentatives: Set[T] = getClasses.keySet

    def getClass(rep: T): Option[Set[T]] = getClasses.get(rep)
  }

  trait EquivalenceRelation[R, T] {
    def createClasses(param: R): EquivalenceClasses[T]
  }

  val placeholderString = "Placeholder"

  def createPlaceholder(placeholderType: AnnotatedTree, i: Int): Identifier =
    Identifier(literal = s"${placeholderString}_${i}_type_${Programs.termToString(placeholderType)}",
      annotation = Some(placeholderType))

  def inductionVar(varType: AnnotatedTree): Identifier = {
    createPlaceholder(varType, 0)
  }
}
