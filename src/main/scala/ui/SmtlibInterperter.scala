package ui

import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.TheoryExplorationAction
import transcallang.{AnnotatedTree, Datatype, Language}

class SmtlibInterperter {
  val state = new ActionSearchState(Programs.empty, Set.empty)
  val datatypes = collection.mutable.Set.empty[Datatype]
  val knownFunctions = collection.mutable.Set.empty[AnnotatedTree]
  var goal: Option[(AnnotatedTree, AnnotatedTree)] = None

  def apply(terms: List[AnnotatedTree]) = {
    for(t <- terms) {
      t.root match {
        case Language.functionDeclId =>
          // This is a typed identifier for a function type
          knownFunctions += t.subtrees.head
        case Language.datatypeId =>
          // TODO: implement type parameters
          datatypes += Datatype(t.subtrees.head.root, Seq.empty, t.subtrees.tail.map(_.root))
        case Language.letId =>
          new LetAction(t)(state)
        case Language.assertId =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0), t.subtrees.head.subtrees(1)))
          // TODO: use the code for automatic examples + move to using sorted vocab as input
          throw new NotImplementedError("Need to apply theory exploration here")
//          new TheoryExplorationAction(datatypes.flatMap(_.constructors).toSet, knownFunctions.toSet, )
          // TODO: TheSy should have support for stopping upon finding specific lemma
      }
    }
  }
}
