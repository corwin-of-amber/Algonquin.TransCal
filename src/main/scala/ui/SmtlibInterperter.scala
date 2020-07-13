package ui

import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.{SortedVocabulary, TheoryExplorationAction}
import synthesis.search.rewrites.RewriteRule
import transcallang.{AnnotatedTree, Datatype, Identifier, Language}

class SmtlibInterperter {
  val state = new ActionSearchState(Programs.empty, Set.empty[RewriteRule])
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
          val vocab = SortedVocabulary(datatypes.toSeq, knownFunctions.toSeq)
          val thesy = new TheoryExplorationAction(vocab, 3, None, None, None, None, None, true)
          thesy.addGoal(goal.get)
          thesy(state)
        case Identifier("not", annotation, namespace) if t.subtrees.head.root == Language.letId =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0), t.subtrees.head.subtrees(1)))
          val vocab = SortedVocabulary(datatypes.toSeq, knownFunctions.toSeq)
          val thesy = new TheoryExplorationAction(vocab, 3, None, None, None, None, None, true)
          thesy.addGoal(goal.get)
          thesy(state)
        case x => throw new NotImplementedError("Check this")
      }
    }
  }
}
