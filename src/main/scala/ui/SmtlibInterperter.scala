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
  val state = new ActionSearchState(Programs.empty, Set.empty[RewriteRule])
  val datatypes = collection.mutable.Set.empty[Datatype]
  val knownFunctions = collection.mutable.Set.empty[AnnotatedTree]
  var goal: Option[(AnnotatedTree, AnnotatedTree)] = None
  val knownDefs = collection.mutable.Set.empty[AnnotatedTree]

  //  val ois = new ObjectInputStream(new FileInputStream("temp"))
  //  var obj: AnyRef = null
  //  var cont = true
  //  while (cont) {
  //    try {
  //      obj = ois.readObject()
  //    } catch {
  //      case _: Throwable => cont = false
  //    }
  //    assert(obj != null)
  //    obj match {
  //      case x: AnnotatedTree => println(s"${Programs.termToString(x)}")
  //    }
  //  }
  def runExploration(t: AnnotatedTree, oosPath: String, previousResults: Set[RunResults]) = {
    assert(t.subtrees.head.root == Language.letId)

    val relevantResults = previousResults.filter(rr => rr.knownRulesDefs.diff(knownDefs).isEmpty && rr.knownTypes.diff(datatypes).isEmpty)
    for (rr <- relevantResults) {
      state.addRules(rr.newRules.flatMap(new LetAction(_, allowExistential = false).rules))
    }

    def cleanAutovar(identifier: Identifier) =
      if (identifier.literal.startsWith("?autovar")) identifier.copy(literal = identifier.literal.drop(1))
      else identifier

    goal = Some((t.subtrees.head.subtrees(0).map(cleanAutovar), t.subtrees.head.subtrees(1).map(cleanAutovar)))
    val vocab = SortedVocabulary(datatypes.toSeq, knownFunctions.toSeq)
    val exampleDepth = 3
    val distributer = Distributer(vocab, exampleDepth)
    distributer.runTasks(state)
    val thesy = new TheoryExplorationAction(vocab, exampleDepth, None, None, None, None, None, true)
    thesy.addGoal(goal.get)
    thesy(state)
    val res = RunResults(datatypes.toSet, knownFunctions.toSet, knownDefs.toSet, distributer.foundRules ++ thesy.getFoundRules, goal, thesy.goals.isEmpty)
    val oos = new ObjectOutputStream(new FileOutputStream(oosPath))
    oos.writeObject(res)
    oos.close()
    res
  }

  def apply(terms: List[AnnotatedTree], oos: String, previousResults: Set[RunResults] = Set.empty): RunResults = {
    var res: Option[RunResults] = None
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
          val letAction: LetAction = new LetAction(t, allowExistential = false)
          letAction(state)
        case Language.assertId =>
          res = Some(runExploration(t, oos, previousResults))
        case Identifier("not", annotation, namespace) if t.subtrees.head.root == Language.letId =>
          res = Some(runExploration(t, oos, previousResults))
        case x => throw new NotImplementedError("Check this")
      }
    }
    res.get
  }
}
