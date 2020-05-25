package synthesis.search.action.operators.thesy

import com.typesafe.scalalogging.LazyLogging
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.search.action.ActionSearchState.HyperGraph
import synthesis.search.action.operators.{CaseSplitAction, ObservationalEquivalence, SearchAction}
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.{FunctionArgumentsAndReturnTypeRewrite, RewriteRule}
import synthesis.search.rewrite.operators.Template.ReferenceTerm
import transcallang.{AnnotatedTree, Datatype, Identifier, Language}

import scala.collection.mutable

class ConjectureGenerator(vocab: SortedVocabulary,
                          searcher: SearchAction,
//                          exampleDepthLimit: Int,
                          examples: Map[AnnotatedTree, Seq[AnnotatedTree]],
                          placeholderCount: Int) extends LazyLogging {
  //TODO: remove placeholder count and:
  // TODO: 1. Dynamically decide phC per type by arity of functions
  // TODO: 2. Generate expression using a pattern and a factory. See Issue 13
  val typed = true

  private def isFunctionType(annotatedTree: AnnotatedTree) = annotatedTree.root.annotation match {
    case Some(annotation) => annotation.root == Language.mapTypeId
    case None => false
  }

  private val types: Set[AnnotatedTree] =
    vocab.datatypes.flatMap(d => d.constructors.map(_.annotation.get)).toSet ++ vocab.definitions.flatMap(_.getType)

  private def createPlaceholder(placeholderType: AnnotatedTree, i: Int): Identifier =
    Identifier(literal = s"Placeholder_${i}_type_${Programs.termToString(placeholderType)}",
      annotation = Some(placeholderType))

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

  // TODO: enable this after finihing refactor
  // TODO: Might want a better way of building then randomly taking previously built examples.
  // TODO: I feel like beuilding all veriations will be very expensive but actuall
//  private val examples: Map[AnnotatedTree, Seq[AnnotatedTree]] = vocab.datatypes.map(d => {
//    val all: mutable.Buffer[Set[AnnotatedTree]] = mutable.Buffer[Set[AnnotatedTree]]
//    all(0) = d.constructors.filterNot(c => isFunctionType(c._2)).map(t => AnnotatedTree.identifierOnly(t._1)).toSet
//    val functionConstructors = d.constructors.filter(c => isFunctionType(c._2))
//    for (i <- 1 to exampleDepthLimit; exToUse = mutable.Set(all(i - 1).toSeq: _*)) {
//      val newExs = functionConstructors.map(c => {
//        AnnotatedTree.withoutAnnotations(c._1, c._2.subtrees.map({
//          case d.asType =>
//            if (exToUse.nonEmpty) {
//              val temp = exToUse.head
//              exToUse -= temp
//              temp
//            }
//            else all(i - 1).head
//          case t =>
//            createAutoVar(t)
//        }))
//      })
//      all(i) = newExs.toSet
//    }
//    (d.asType, all.flatten)
//  }).toMap

  private val sygusRules = {
    // Need to use vocab as the function name is needed
    SyGuSRewriteRules(
      (vocab.allSymbols.toSet).filter(isFunctionType)
        .map(t => t.copy(subtrees = Seq.empty))
    ).rewriteRules.map(_.asInstanceOf[RewriteRule])
  }

  private val baseState: RewriteSearchState = {
    val symbols = vocab.allSymbols ++ placeholders.values.flatMap(ps => ps.map(AnnotatedTree.identifierOnly))
    val symbolsToUse = symbols.map(t => AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(
      AnnotatedTree.identifierOnly(Language.trueId),
      AnnotatedTree.withoutAnnotations(SyGuSRewriteRules.sygusCreatedId, Seq(t))
    )))
    val tree = if (symbols.size > 1)
      AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, symbolsToUse)
    else
      symbolsToUse.head
    val res =
      if (typed) Programs.destruct(tree)
      else Programs.destruct(tree.cleanTypes)
    new RewriteSearchState(res)
  }

  def increaseDepth(): Unit = {
    // TODO: Run new rules before inreasing depth
    // TODO: Keep same soes
    val op = new Operator[RewriteSearchState] {
      override def apply(state: RewriteSearchState): RewriteSearchState = {
        val hyperTermIds: Seq[() => HyperTermId] = 0 until (sygusRules.size + 1) map (i => {
          val creator = Stream.from(if (state.graph.isEmpty) i else state.graph.nodes.map(_.id).max + 1 + i, sygusRules.size).map(HyperTermId).iterator
          () => creator.next
        })

        val res = sygusRules.par.map((r: RewriteRule) => r.getStep(state, versioned = false))
        val newEdges = res.zip(hyperTermIds).map({ case (es, idCreator) => structures.generic.HyperGraph.fillWithNewHoles(es, idCreator) }).seq.flatten
        logger.debug(s"Found ${newEdges.size} new edges using sygus")
        state.graph.addAllKeepVersion(newEdges)
        val funcInferStep = FunctionArgumentsAndReturnTypeRewrite.getStep(state, versioned = false)
        state.graph.addAllKeepVersion(structures.generic.HyperGraph.fillWithNewHoles(funcInferStep, hyperTermIds.last))
        state
      }
    }
//    for (soe <- soes) {
//      soe.updateGraph(op)
//    }
    op(baseState)
  }

  def inferConjectures(operators: Set[Operator[RewriteSearchState]]) = {
    val soes = vocab.datatypes.map(d => new SOE(searcher, baseState, placeholders(d.asType).head, examples(d.asType)))
    val idsToMerge = ObservationalEquivalence.flattenUnionConclusions(soes.map(_.findEquives(operators.toSeq)))
    ObservationalEquivalence.mergeConclusions(baseState, idsToMerge.toSeq)
  }

  def inductionVar(varType: AnnotatedTree): Identifier = {
    placeholders(varType).head
  }
}
