package ui

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.{Distributer, SortedVocabulary, TheoryExplorationAction}
import synthesis.search.rewrites.RewriteRule
import transcallang.{AnnotatedTree, Datatype, Identifier, Language}
import ui.Main.conf

class SmtlibInterperter {
  def runExploration(vocab: SortedVocabulary, goals: Set[(AnnotatedTree, AnnotatedTree)], knownDefs: Set[AnnotatedTree], phCount: Int, oosPath: String, previousResults: Set[RunResults], reprove: Boolean = true) = {
    val state = prepareState(vocab, knownDefs, previousResults)
    val exampleDepth = 3
    val termDepth = Some(2)
//    val distributer = Distributer(vocab, exampleDepth)
//    distributer.runTasks(state)
    val thesy = new TheoryExplorationAction(vocab, exampleDepth, termDepth, None, None, None, Some(phCount), false)

    thesy.setTimingBasename(new File(oosPath).getName + "_")
    goals.foreach(g => thesy.addGoal(g))
    thesy(state)
    goalReport(goals, thesy)

    var res = RunResults(vocab.datatypes.toSet, vocab.definitions.toSet, knownDefs.toSet, thesy.getFoundRules, goals, goals.diff(thesy.goals))
    if (phCount > 3) {
      val thesy2 = new TheoryExplorationAction(vocab, exampleDepth, termDepth, None, None, None, Some(2), false)
      goals.foreach(g => thesy.addGoal(g))
      thesy2(state)
      res = RunResults(vocab.datatypes.toSet, vocab.definitions.toSet, knownDefs.toSet, thesy.getFoundRules ++ thesy2.getFoundRules, goals, goals.diff(thesy2.goals))
    }

    val oos = new ObjectOutputStream(new FileOutputStream(oosPath))
    oos.writeObject(res)
    oos.close()
    res
  }

  def justCheck(vocab: SortedVocabulary, goals: Set[(AnnotatedTree, AnnotatedTree)], knownDefs: Set[AnnotatedTree], previousResults: Set[RunResults]) = {
    val state = prepareState(vocab, knownDefs, previousResults)
    val thesy = new TheoryExplorationAction(vocab, 3, Some(0), None, None, None, Some(1), true)
    goals.foreach(g => thesy.addGoal(g))
    thesy(state)
    thesy.checkGoals(state, null)
    goalReport(goals, thesy)
  }

  def prepareState(vocab: SortedVocabulary, knownDefs: Set[AnnotatedTree], previousResults: Set[RunResults]) = {
    val relevantResults = previousResults.filter(rr => rr.knownRulesDefs.diff(knownDefs).isEmpty && rr.knownTypes.diff(vocab.datatypes.toSet).isEmpty)

    val state = new ActionSearchState(Programs.empty, Set.empty[RewriteRule])
    knownDefs.foreach(t => new LetAction(t, allowExistential = false)(state))
    for (rr <- relevantResults) {
      state.addRules(rr.newRules.flatMap(new LetAction(_, allowExistential = false).rules))
    }
    state
  }

  def goalReport(goals: Set[(AnnotatedTree, AnnotatedTree)], thesy: TheoryExplorationAction): Unit = {
    for ((x, y) <- goals -- thesy.goals)
      println(s" ✓ ${Programs.termToString(x)} = ${Programs.termToString(y)}")
    for ((x, y) <- thesy.goals)
      println(s" ✗ ${Programs.termToString(x)} = ${Programs.termToString(y)}")

    println(if (thesy.goals.isEmpty) "Proved all goals :)" else " Some goals remain :(")
  }

  def ctorsId(datatypes: Set[Datatype]): Map[Identifier, Datatype] = datatypes.flatMap(d => d.constructors.map(_.copy(annotation = None) -> d)).toMap

  def datatypesId(datatypes: Set[Datatype]): Map[Identifier, Datatype] = datatypes.map(d => d.name -> d).toMap

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
          assert(t.subtrees.head.isLeaf)
          val fsymbol = t.subtrees.head
          knownFunctions += fsymbol
        case Language.datatypeId =>
          // TODO: implement type parameters
          val dt = Datatype(t.subtrees.head.root, Seq.empty, t.subtrees.tail.map(_.root))
          datatypes += dt
        case Language.letId =>
          knownDefs += t
        case Language.assertId =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0).map(cleanAutovar), t.subtrees.head.subtrees(1).map(cleanAutovar)))
        case Identifier("not", annotation, namespace) if t.subtrees.head.root == Language.letId =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0).map(cleanAutovar), t.subtrees.head.subtrees(1).map(cleanAutovar)))
        case Identifier("not", annotation, namespace) if t.subtrees.head.root != Language.letId =>
          throw new IllegalArgumentException(s"Don't support stuff that isn't equality (${t.subtrees.head})")
        case x =>
          throw new NotImplementedError("Check this")
      }
    }

    val vocab = SortedVocabulary(
      usedDatatypes(datatypes.toSet, knownDefs.toSet),
      defdFunctions(knownFunctions.toSet, knownDefs.toSet))
    assert(vocab.definitions.nonEmpty, "no function definitions found")
    assert(vocab.datatypes.nonEmpty, "no relevant datatypes found")
    println(vocab.prettyPrint)

    val splits = knownDefs.flatMap(guessCaseSplits)
    for ((head, on, cases) <- splits)
      println(s"${Programs.termToString(head)} ---> ${Programs.termToString(on)}\n   ${cases.map(Programs.termToString).mkString(" / ")}")
    (vocab, knownDefs.toSet, Set(goal.get))
  }

  def apply(terms: List[AnnotatedTree], oos: String, previousResults: Set[RunResults] = Set.empty): RunResults = {
    val (vocab, funcDefs, goals) = toVocabAndGoals(terms)
    runExploration(vocab, goals, funcDefs, 2, oos, previousResults)
  }

  def check(terms: List[AnnotatedTree], previousFilenames: Seq[String]): Unit =
    check(terms, readPreviousResults(previousFilenames))

  def check(terms: List[AnnotatedTree], previousResults: Set[RunResults] = Set.empty): Unit = {
    val (vocab, defs, goals) = toVocabAndGoals(terms)
    for (rr <- previousResults; ruleDef <- rr.newRules)
      println(s"  ${Programs.termToString(ruleDef)}")
    for ((x, y) <- goals) {
      println(s" → ${Programs.termToString(x)} =? ${Programs.termToString(y)}")
    }
    justCheck(vocab, goals, defs, previousResults)
  }

  def readPreviousResults(filenames: Seq[String]): Set[RunResults] = {
    filenames.map(fn => {
      val ois = new ObjectInputStream(new FileInputStream(fn))
      try { ois.readObject().asInstanceOf[RunResults] } finally { ois.close() }
    }).toSet
  }

  def defdFunctions(funDecls: Set[AnnotatedTree], funDefs: Set[AnnotatedTree]): Set[AnnotatedTree] = {
    assert(funDecls.forall(_.isLeaf))
    assert(funDefs.forall(_.root == Language.letId))
    funDefs.flatMap(t => definesWhatFunctions(funDecls, t)).toSet
  }

  def usedDatatypes(datatypes: Set[Datatype], funDefs: Set[AnnotatedTree]): Set[Datatype] = {
    assert(funDefs.forall(_.root == Language.letId))
    funDefs.flatMap(t => usesWhatDatatypes(datatypes, t)).toSet
  }

  def definesWhatFunctions(knownFuncs: Set[AnnotatedTree], t: AnnotatedTree): Seq[AnnotatedTree] = {
    assert(knownFuncs.forall(_.isLeaf))
    val namesToFuncs = knownFuncs.map(f => f.root.literal -> f).toMap
    t.root match {
      case i: Identifier if Language.builtinDefinitions.contains(i) =>
        t.nodes.map(_.root).flatMap(i => namesToFuncs.get(i.literal))
      case _ => Seq.empty
    }
  }

  def usesWhatDatatypes(datatypes: Set[Datatype], t: AnnotatedTree): Stream[Datatype] = {
    val ctorsIdsMap = ctorsId(datatypes)
    t.nodes.flatMap(s => ctorsIdsMap.get(s.root).toSeq ++ typeOf(datatypes, s).toSeq)
  }

  def typeOf(datatypes: Set[Datatype], t: AnnotatedTree): Option[Datatype] = {
    // TODO: this seems stupid
    val datatypesIdsMap = datatypesId(datatypes)
    t.getRetType.flatMap(ty => datatypesIdsMap.get(ty.root))
  }

  def guessCaseSplits(t: AnnotatedTree) = {
    t match {
      case AnnotatedTree(i, Seq(lhs, rhs), _) if Language.builtinDefinitions.contains(i) =>
        for ((on, cases) <- findItes(rhs)) yield (lhs, on, cases)
        // @todo replace findItes with findMatches (pending transSExp)
      case _ => Seq.empty
    }
  }

  val BOOLEAN_CASES = Seq(Language.trueId, Language.falseId).map(AnnotatedTree.identifierOnly)

  def findItes(t: AnnotatedTree) = {
    t.nodes.flatMap(s =>
      if (s.root.literal == "ite") Some(s.subtrees.head, BOOLEAN_CASES)
      else None)
  }

  def findMatches(t: AnnotatedTree): Stream[(AnnotatedTree, Seq[Identifier])] = {
    println(t)
    t.nodes.flatMap(s =>
      if (s.root == Language.matchId) Some(s.subtrees.head, Seq(???))
      else None)
  }
}
