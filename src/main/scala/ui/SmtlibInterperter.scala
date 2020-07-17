package ui

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import structures.{EmptyMetadata, Metadata}
import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.{Distributer, SortedVocabulary, TheoryExplorationAction}
import synthesis.search.rewrites.{RewriteRule, RewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{AnnotatedTree, Datatype, Identifier, Language, TranscalParser}

object BaseTheoryRulesDb extends RewriteRulesDB {
  val parser = new TranscalParser
  override protected def ruleTemplates: Set[AnnotatedTree] = Set(
    "ite ?b ?x ?y |>> splitTrue ||| possibleSplit(b, true, false)",
    "ite true ?x ?y >> x",
    "ite false ?x ?y >> y",
    "and ?x ?y = ite x (ite y true false) false",
    "and ?x ?y >> and y x",
    "or ?x ?y = ite x true (ite y true false)",
    "or ?x ?y >> or y x",
    "not ?x = ite x false true",
    "implication ?x ?y = or y (not x)"
    ).map(s => parser.apply(s))

  override protected def metadata: Metadata = EmptyMetadata
}

class SmtlibInterperter {
  def runExploration(vocab: SortedVocabulary, goals: Set[(AnnotatedTree, AnnotatedTree)], knownDefs: Set[AnnotatedTree], phCount: Int, oosPath: String, previousResults: Set[RunResults], reprove: Boolean) = {
    val state = prepareState(vocab, knownDefs, previousResults)
    val needsSpliting = knownDefs.exists(t => t.nodes.exists(n => n.root.literal == "ite"))
    val exampleDepth = if(needsSpliting) 2 else 3
    val fixedPhCount = if(needsSpliting && phCount > 2) 2 else phCount
    val termDepth = Some(2)
//    val distributer = Distributer(vocab, exampleDepth)
//    distributer.runTasks(state)

    val thesy = new TheoryExplorationAction(vocab, exampleDepth, termDepth, None, None, None, Some(fixedPhCount), reprove)

    thesy.setTimingBasename(new File(oosPath).getName + "_")
    goals.foreach(g => thesy.addGoal(g))
    thesy(state)
    goalReport(goals, thesy)

    var res = RunResults(vocab.datatypes.toSet, vocab.definitions.toSet, knownDefs.toSet, thesy.getFoundRules, goals, goals.diff(thesy.goals))
    if (fixedPhCount > 2) {
      val thesy2 = new TheoryExplorationAction(vocab, exampleDepth, termDepth, None, None, None, Some(2), reprove)
      goals.foreach(g => thesy2.addGoal(g))
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
    thesy.checkGoals(state, thesy.lastProver.get)
    goalReport(goals, thesy)
  }

  def prepareState(vocab: SortedVocabulary, knownDefs: Set[AnnotatedTree], previousResults: Set[RunResults]) = {
    val relevantResults = previousResults

    val state = new ActionSearchState(Programs.empty, SystemRewriteRulesDB.rewriteRules ++ BaseTheoryRulesDb.rewriteRules)
    knownDefs.foreach(t => new LetAction(t, allowExistential = false)(state))
    for (rr <- relevantResults) {
      state.addRules(rr.newRules.flatMap(new LetAction(_, allowExistential = false).rules))
      println(s"Added rule from previous result: \n${rr.newRules.map(Programs.termToString).mkString("\n")}")
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

    for ((t, i) <- (terms.dropRight(1) :+ AnnotatedTree.withoutAnnotations(Language.assertId, List(terms.last))).zipWithIndex) {
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
          assert(t.subtrees.head.root.literal == "not")
          val goalT = t.subtrees.head.subtrees.head
          if (goalT.root == Language.letId)
            goal = Some((goalT.subtrees(0), goalT.subtrees(1)))
          else
            goal = Some((AnnotatedTree.identifierOnly(Language.trueId), goalT))
        case Identifier("not", annotation, namespace) if t.subtrees.head.root == Language.letId && i == terms.length - 1 =>
          assert(t.subtrees.head.root == Language.letId)
          goal = Some((t.subtrees.head.subtrees(0), t.subtrees.head.subtrees(1)))
        case x =>
          val term = AnnotatedTree.withoutAnnotations(Language.letId, List(AnnotatedTree.identifierOnly(Language.trueId), t))
          println(s"warning! CHECK THIS TERM!!! ${Programs.termToString(term)}")
          knownDefs += term
        case x =>
          throw new NotImplementedError(s"Check this $x")
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
    try {
      val (vocab, funcDefs, goals) = toVocabAndGoals(terms)
      runExploration(vocab, goals, funcDefs, 2, oos, previousResults, true)
    } catch {
      case e => println(s"***** ERROR IN $oos - $e *****")
        throw e
    }
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
