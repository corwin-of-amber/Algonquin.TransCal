package ui

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import structures.mutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.search.ActionSearchState
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.{Distributer, SortedVocabulary, TheoryExplorationAction}
import synthesis.search.rewrites.{PatternRewriteRule, RewriteRule}
import synthesis.search.rewrites.Template.TemplateTerm
import transcallang.{AnnotatedTree, Datatype, Identifier, Language}

class SmtlibInterperter {
  def runExploration(vocab: SortedVocabulary, goals: Set[(AnnotatedTree, AnnotatedTree)], knownDefs: Set[AnnotatedTree], oosPath: String, previousResults: Set[RunResults]) = {
    val relevantResults = previousResults.filter(rr => rr.knownRulesDefs.diff(knownDefs).isEmpty && rr.knownTypes.diff(vocab.datatypes.toSet).isEmpty)

    val state = new ActionSearchState(Programs.empty, Set.empty[RewriteRule])
    knownDefs.foreach(t => new LetAction(t, allowExistential = false)(state))
    for (rr <- relevantResults) {
      state.addRules(rr.newRules.flatMap(new LetAction(_, allowExistential = false).rules))
    }
    val exampleDepth = 3
    val distributer = Distributer(vocab, exampleDepth)
    distributer.runTasks(state)
    val thesy = new TheoryExplorationAction(vocab, exampleDepth, None, None, None, None, None, true)
    goals.foreach(g => thesy.addGoal(g))
    thesy(state)
    val res = RunResults(vocab.datatypes.toSet, vocab.definitions.toSet, knownDefs.toSet, distributer.foundRules ++ thesy.getFoundRules, goals, goals.diff(thesy.goals))
    val oos = new ObjectOutputStream(new FileOutputStream(oosPath))
    oos.writeObject(res)
    oos.close()
    res
  }

  def toVocabAndGoals(terms: List[AnnotatedTree]): (SortedVocabulary, Set[AnnotatedTree], Set[(AnnotatedTree, AnnotatedTree)]) = {
    val datatypes = collection.mutable.Set.empty[Datatype]
    val knownFunctions = collection.mutable.Set.empty[AnnotatedTree]
    var goal: Option[(AnnotatedTree, AnnotatedTree)] = None
    val knownDefs = collection.mutable.Set.empty[AnnotatedTree]

    def cleanAutovar(identifier: Identifier) =
      if (identifier.literal.startsWith("?autovar")) identifier.copy(literal = identifier.literal.drop(1))
      else identifier

    for (t <- terms) {
      t.root match {
        case Language.functionDeclId =>
          // This is a typed identifier for a function type
          knownFunctions += t.subtrees.head
        case Language.datatypeId =>
          // TODO: implement type parameters
          datatypes += Datatype(t.subtrees.head.root, Seq.empty, t.subtrees.tail.map(_.root))
        case Language.letId =>
          knownDefs += t
        case Language.assertId =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0).map(cleanAutovar), t.subtrees.head.subtrees(1).map(cleanAutovar)))
        case Identifier("not", annotation, namespace) if t.subtrees.head.root == Language.letId =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0).map(cleanAutovar), t.subtrees.head.subtrees(1).map(cleanAutovar)))
        case Identifier("not", annotation, namespace) if t.subtrees.head.root != Language.letId =>
          throw new IllegalArgumentException("Don't support stuff that isn't equality")
        case x =>
          throw new NotImplementedError("Check this")
      }
    }
    val vocab = SortedVocabulary(datatypes.toSeq, knownFunctions.toSeq)
    (vocab, knownDefs.toSet, Set(goal.get))
  }

  def apply(terms: List[AnnotatedTree], oos: String, previousResults: Set[RunResults] = Set.empty): RunResults = {
    val (vocab, funcDefs, goals) = toVocabAndGoals(terms)
    runExploration(vocab, goals, funcDefs, oos, previousResults)
  }
}
