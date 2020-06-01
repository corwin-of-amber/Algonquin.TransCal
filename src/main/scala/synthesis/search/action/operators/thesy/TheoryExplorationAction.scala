package synthesis.search.action.operators.thesy

import report.StopWatch
import structures.{EmptyMetadata, Metadata}
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.search.action.ActionSearchState.HyperGraph
import synthesis.search.action.operators._
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.Template.ReferenceTerm
import synthesis.search.rewrite.operators.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Datatype, Identifier, Language, TranscalParser}

import scala.collection.mutable

/** Theory exploration powered by term rewriting
  *
  * This works by applying a few actions iteratively, acheiving an iterative deepening of the hyper graph.
  *
  * Create sygue rules only from constructors and grammer (once).
  * Create placeholders ahead of time (once).
  * Base graph of placeholders and constants of created type (once).
  * Create next depth by applying sygue rules.
  * Run soe to find potential conjectures.
  * Filter trivial conjectures.
  * Prove conjectures.
  *
  * @param typeBuilders           Constructors for types being inducted on
  * @param grammar                Function symbols to use as syntax for SyGuE
  * @param examples               Should be replaced by automatic creation of :typeBuilder application
  * @param termDepthOption        Depth to create terms, the iterative deepening depth
  * @param equivDepthOption       Depth to run rewrite rules in both conjecture genreration and prover
  * @param splitDepthOption       Allow nested split case up to this depth
  * @param preRunDepth            When using case split it is more efficient to run rewrite search before each split
  * @param placeholderCountOption How many place holders to create
  */
class TheoryExplorationAction(typeBuilders: Set[Identifier],
                              grammar: Set[AnnotatedTree],
                              examples: Map[AnnotatedTree, Seq[AnnotatedTree]],
                              termDepthOption: Option[Int] = None,
                              equivDepthOption: Option[Int] = None,
                              splitDepthOption: Option[Int] = None,
                              preRunDepth: Option[Int] = None,
                              placeholderCountOption: Option[Int] = None) extends Action {
  assert(examples.values.map(_.size).toSet.size == 1)
  var termCount = 0
  var failedProofs = 0
  var retriedProofs = 0
  val allfailed: mutable.Set[(AnnotatedTree, AnnotatedTree)] = mutable.Set.empty

  val equivDepth = equivDepthOption.getOrElse(4)
  val splitDepth = splitDepthOption.getOrElse(1)
  val termDepth = termDepthOption.getOrElse(2)
  val placeholderCount = placeholderCountOption.getOrElse(2)

  // TODO: change input into sorted vocab
  // TODO: Should be provate, change tests
  val vocab = {
    val baseType = typeBuilders.find(_.annotation.get.root != Language.mapTypeId).flatMap(_.annotation).get
    SortedVocabulary(Seq(Datatype(baseType.root, baseType.subtrees, typeBuilders.toSeq)), grammar.toSeq)
  }

  // TODO: Should be provate, change tests
  val searcher = {
    val randomChooser = CaseSplitAction.randomChooser(equivDepth, splitDepth)
    new OperatorRunWithCaseSplit(equivDepth, splitDepth = Some(splitDepth), chooser = Some(randomChooser), preRunDepth = preRunDepth)
  }

  // TODO: fix tests and make this private
  val conjectureGenerator = new ConjectureGenerator(vocab, searcher, examples, placeholderCount)

  override def apply(state: ActionSearchState): ActionSearchState = {
    val prover = new Prover(vocab.datatypes.toSet, searcher, state.rewriteRules)
    val conjectureChecker = new ConjectureChecker(prover, searcher)
    val foundRules = mutable.Buffer.empty[mutable.Buffer[AnnotatedTree]]
    var newRules = Set.empty[AnnotatedTree]

    for (i <- 1 to termDepth) {
      logger.warn(s"Running SPBE in depth ${i}  @  ${StopWatch.instance.now}")
      logger.info(s"Creating terms of depth $i")
      conjectureGenerator.increaseDepth()
      // ******** SPBE ********
      foundRules += mutable.Buffer.empty
      // Gives a graph of depth i+~ applications of funcs on known terminals and functions
      // Because we merge in the graph there is no need to remember equivalences already found

      logger.warn(s"Finished term creation depth ${i}  @  ${StopWatch.instance.now}")
      logger.info(s"Trying to merge terms")
      val equivalenceClasses = conjectureGenerator.inferConjectures(state.rewriteRules)
      logger.warn(s"Finished symbolic term evaluation depth ${i}  @  ${StopWatch.instance.now}")
      // Prove equivalence by induction.
      logger.info(s"Working on equivalences")
      // Different context for temp names
      val classes: Set[Set[AnnotatedTree]] =
        Programs.reconstructAll(equivalenceClasses.graph, i)
          .groupBy(_.edge.target).values.toSet
          .map((s: Set[Programs.Entry]) => s.map(_.tree))
      logger.warn(s"Finished finding rules depth ${i}  @  ${StopWatch.instance.now}")
      newRules = conjectureChecker.checkConjectures(classes)
      foundRules.last ++= newRules
      logger.info(s"Found new lemmas in depth $i:")
      for (t <- foundRules.last)
        logger.info(s"  ${Programs.termToString(t.subtrees.head)} = ${Programs.termToString(t.subtrees.last)}")
    }

    logger.info(s"Searching for rules that have become provable:")
    var continue = 0
    if (newRules.nonEmpty) {
      val equivalenceClasses = conjectureGenerator.inferConjectures(state.rewriteRules)
      do {
        // Reset versioning to look only on results from new rules.
        val classes: Set[Set[AnnotatedTree]] = Programs.reconstructAll(equivalenceClasses.graph, termDepth)
          .groupBy(_.edge.target).values.toSet
          .map((s: Set[Programs.Entry]) => s.map(_.tree))
        newRules = conjectureChecker.checkConjectures(classes)
        continue += 1
        logger.warn(s"Finished finding rules repeat $continue depth ${termDepth}  @  ${StopWatch.instance.now}")
        for (t <- foundRules.last)
          logger.info(s"  ${Programs.termToString(t.subtrees.head)} = ${Programs.termToString(t.subtrees.last)}")
        foundRules.last ++= newRules
      } while (newRules.nonEmpty)
    }
    logger.info("Done searching for rules:")
    for (t <- foundRules.flatten)
      logger.info(s"  ${Programs.termToString(t.subtrees.head)} = ${Programs.termToString(t.subtrees.last)}")
    //    }

    logger.info(s"Done SPBE  @  ${StopWatch.instance.now}")
    logger.info(s"term count: $termCount")
    logger.info(s"failed count: $failedProofs")
    logger.info(s"retry success count: $retriedProofs")
    state.copy(rewriteRules = prover.knownRules)
  }
}