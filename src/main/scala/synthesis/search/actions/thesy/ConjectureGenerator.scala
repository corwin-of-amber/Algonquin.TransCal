package synthesis.search.actions.thesy

import com.typesafe.scalalogging.LazyLogging
import report.LazyTiming
import synthesis.search.actions.thesy.SyGuERewriteRules.{SyGuEMetadata, SyGuEMetadataLeaf}
import synthesis.search.actions.{Action, ObservationalEquivalence}
import synthesis.search.rewrites.{FunctionArgumentsAndReturnTypeRewrite, PatternRewriteRule, RewriteRule}
import synthesis.search.{ActionSearchState, Operator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.collection.mutable

class ConjectureGenerator(vocab: SortedVocabulary,
                          searcher: Action,
                          exampleDepthLimit: Int,
                          //                          examples: Map[AnnotatedTree, Seq[AnnotatedTree]],
                          placeholderCount: Int) extends LazyLogging with LazyTiming {
  //TODO: remove placeholder count and:
  // TODO: 1. Dynamically decide phC per type by arity of functions
  // TODO: 2. Generate expression using a pattern and a factory. See Issue 13
  val typed = true

  private def isRecursiveType(identifier: Identifier) = identifier.annotation match {
    case Some(annotation) => annotation.root == Language.mapTypeId && annotation.subtrees.dropRight(1).contains(annotation.subtrees.last)
    case None => false
  }

  private val types: Set[AnnotatedTree] =
    vocab.datatypes.flatMap(d => d.constructors.map(_.annotation.get)).toSet ++ vocab.definitions.flatMap(_.getType)

  private val createAutoVar: AnnotatedTree => AnnotatedTree = {
    val counter = mutable.Map.empty[AnnotatedTree, Int].withDefault(_ => 0)
    (varType: AnnotatedTree) => {
      counter(varType) = counter(varType) + 1
      AnnotatedTree.identifierOnly(Identifier(
        literal = s"examplevar_${counter(varType)}_type_{${Programs.termToString(varType)}}",
        annotation = Some(varType)))
    }
  }

  private val placeholders: Map[AnnotatedTree, Seq[Identifier]] =
    types.flatMap({
      case AnnotatedTree(Language.mapTypeId, subtrees, _) => subtrees
      case i => Seq(i)
    }).map(t => (t, 0 until placeholderCount map (i => createPlaceholder(t, i)))).toMap
  logger.warn(s"Created total: ${placeholders.values.flatten.size}")

  private val examples: Map[AnnotatedTree, Seq[AnnotatedTree]] = {
    val rand = new scala.util.Random(1)
    vocab.datatypes.map(d => {
      val all: Seq[mutable.Set[AnnotatedTree]] = 0 until exampleDepthLimit map (_ => mutable.Set.empty[AnnotatedTree])
      // This is important so the prover will be sound
      all(0) ++= (d.constructors.filterNot(c => isRecursiveType(c)).map(t => AnnotatedTree.identifierOnly(t)).toSet)
      // Need to take many variations so wont fall with stuff like reverse tree
      val functionConstructors = d.constructors.filter(c => isRecursiveType(c))
      def createExample(const: Identifier, depth: Int): AnnotatedTree = {
        if (depth == 0) all(0).toSeq(rand.nextInt(all(0).size))
        else {
          val example = AnnotatedTree.withoutAnnotations(const, const.annotation.get.subtrees.dropRight(1).map({
            case t if t == d.asType =>
              createExample(functionConstructors(rand.nextInt(functionConstructors.size)), depth - 1)
            case t =>
              createAutoVar(t)
          }))
          all(depth - 1) += example
          example
        }
      }
      functionConstructors.foreach(createExample(_, exampleDepthLimit))
      (d.asType, all.flatten)
    }).toMap
  }

  private val sygueRules = {
    // Need to use vocab as the function name is needed
    SyGuERewriteRules(
      (vocab.allSymbols.toSet).filter(s => isRecursiveType(s.root))
        .map(t => t.copy(subtrees = Seq.empty))
    ).rewriteRules.map(_.asInstanceOf[PatternRewriteRule])
  }

  // TODO: It would be way better if this was a state
  private val baseGraph: ActionSearchState.HyperGraph = {
    val symbols = vocab.allSymbols ++ placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly))
    val symbolsToUse = symbols.map(t => AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(
      AnnotatedTree.identifierOnly(Language.trueId),
      AnnotatedTree.withoutAnnotations(SyGuERewriteRules.sygueCreatedId, Seq(t))
    )))
    val tree = if (symbols.size > 1)
      AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, symbolsToUse)
    else
      symbolsToUse.head
    val res =
      if (typed) Programs.destruct(tree)
      else Programs.destruct(tree.cleanTypes)
    res.findByEdgeType(HyperTermIdentifier(SyGuERewriteRules.sygueCreatedId)).foreach(e =>
      res.updateMetadata(e,
        SyGuEMetadataLeaf(HyperTermIdentifier(Programs.reconstruct(res, e.sources.head).find(_.depth == 0).get.root))
      ))
    res
  }

  //  private val soes = vocab.datatypes.map(d => new SOE(searcher, baseGraph, placeholders(d.asType).head, examples(d.asType)))

  def increaseDepth(): Unit = timed {
    // TODO: Run new rules before inreasing depth
    // TODO: Keep same soes
    def op(graph: ActionSearchState.HyperGraph): Unit = {
      //      sygueRules.foreach(r => r.registerMetadataCreator(soes.head.iterationCreator))
      val hyperTermIds: Seq[() => HyperTermId] = 0 until (sygueRules.size + 1) map (i => {
        val creator = Stream.from(if (graph.isEmpty) i else graph.nodes.map(_.id).max + 1 + i, sygueRules.size).map(HyperTermId).iterator
        () => creator.next
      })

      val res = sygueRules.par.map((r: PatternRewriteRule) => r.getStep(graph, versioned = false))
      val newEdges = res.zip(hyperTermIds).map({ case (es, idCreator) => structures.generic.HyperGraph.fillWithNewHoles(es, idCreator) }).seq.flatten
      logger.debug(s"Found ${newEdges.size} new edges using sygus")
      graph.addAllKeepVersion(newEdges)
      val funcInferStep = FunctionArgumentsAndReturnTypeRewrite.getStep(graph, versioned = false)
      //      sygueRules.foreach(r => r.unregisterMetadataCreator(soes.head.iterationCreator))
      graph.addAllKeepVersion(structures.generic.HyperGraph.fillWithNewHoles(funcInferStep, hyperTermIds.last))
    }
    //    for (soe <- soes) {
    //      soe.updateGraph(op)
    //    }
    op(baseGraph)
    //    sygueRules.foreach(r => r.registerPostprocessor(soes.head.iterationPostprocessor))
    //    soes.foreach(soe => soe.updateGraph(op))
    //    sygueRules.foreach(r => r.unregisterPostprocessor(soes.head.iterationPostprocessor))
  }

  def inferConjectures(operators: Set[RewriteRule]): ActionSearchState = timed {
    val state = new ActionSearchState(baseGraph, operators)
    //    val conclusions = soes.map(_.findEquives(operators))
    //    val sygueMetadataToMerge = ObservationalEquivalence.flattenUnionConclusions(conclusions)
    //    val idsToMerge = {
    //      val metadataToId = baseGraph.findByEdgeType(HyperTermIdentifier(SyGuERewriteRules.sygueCreatedId)).map(e => {
    //          (e.metadata.find(m => m.isInstanceOf[SyGuEMetadata]).get, e)
    //      }).toMap
    //      sygueMetadataToMerge.map(_.flatMap(m => metadataToId.get(m).map(_.sources.head)))
    //    }
    val soes = vocab.datatypes.map(d => new SOE(searcher, state, placeholders(d.asType).head, examples(d.asType)))
    val idsToMerge = ObservationalEquivalence.flattenUnionConclusions(soes.map(_.findEquives(operators)))
    ObservationalEquivalence.mergeConclusions(state, idsToMerge.toSeq)
  }
}
