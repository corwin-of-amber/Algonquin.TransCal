package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.immutable.HyperGraph
import synthesis._
import synthesis.actions.ActionSearchState
import synthesis.complexity.{AddComplexity, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.{RewriteSearchSpace, RewriteSearchState}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.{NaiveSearch, Operator}
import transcallang.{AnnotatedTree, Identifier, Language}

/** Calculate time complex of recursive terms.
  *
  */
class RecursiveTimeComplexAction(function: Identifier, arguments: Int) extends Action with LazyLogging {
  assume(arguments > 0)

  /** The function edge regex to look for. */
  private val functionEdgeRegex = HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(function)), Stream.from(1).map(ReferenceTerm(_)).take(arguments), EmptyMetadata)

  /** The function pattern. */
  private val hyperPattern: HyperPattern = HyperGraph(functionEdgeRegex)

  /** The function hole. */
  private val timeComplexFunction = Identifier("autoFunction")

  /** New function time complex matches. */
  private val hyperPattern1: HyperPattern = {
    val timeComplexTrueNode = ReferenceTerm(0)
    val functionNode = ReferenceTerm(1)
    val tcFNode = ReferenceTerm(2)
    val tcFullNode = ReferenceTerm(3)
    val argumentsRepetition = Stream.from(4).map(ReferenceTerm(_)).take(arguments)
    HyperGraph(
      HyperEdge(timeComplexTrueNode, ExplicitTerm(HyperTermIdentifier(Language.timeComplexTrueId)), Seq.empty, EmptyMetadata),
      HyperEdge(timeComplexTrueNode, ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(functionNode, tcFNode), EmptyMetadata),
      HyperEdge(timeComplexTrueNode, ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(functionNode, tcFullNode), EmptyMetadata),
      HyperEdge(functionNode, ExplicitTerm(HyperTermIdentifier(function)), argumentsRepetition, EmptyMetadata),
      HyperEdge(tcFNode, ExplicitTerm(HyperTermIdentifier(timeComplexFunction)), argumentsRepetition, EmptyMetadata),
    )
  }

  private def findUntouchedTerms(hyperGraph: ActionSearchState.HyperGraph): RewriteSearchState.HyperGraph = {
    var filledHyperGraph = mutable.CompactHyperGraph.empty ++ hyperGraph
    for(
      timeComplexTrue <- hyperGraph.find(_.edgeType.identifier == Language.timeComplexTrueId).map(_.target);
      (idMap, _) <- hyperGraph.findSubgraph[Int](hyperPattern)
    ) if (!hyperGraph.exists(edge => edge.target == timeComplexTrue && edge.edgeType.identifier == Language.timeComplexId && edge.sources.head == idMap(0))) {
      val newNode = HyperTermId(filledHyperGraph.nodes.map(_.id).max + 1)
      val bridgeEdge = HyperEdge(timeComplexTrue, HyperTermIdentifier(Language.timeComplexId), Seq(idMap(0), newNode), EmptyMetadata)
      val timeComplexFunctionEdge = HyperEdge(newNode, HyperTermIdentifier(timeComplexFunction), idMap.filterKeys(_ > 0).toList.sortWith(_._1 < _._1).map(_._2), EmptyMetadata)
      filledHyperGraph ++= Seq(bridgeEdge, timeComplexFunctionEdge)
    }
    filledHyperGraph
  }

  private def populateEdges(hyperGraph: RewriteSearchState.HyperGraph, rewriteRules: Set[Operator[RewriteSearchState]]): RewriteSearchState.HyperGraph = {
    val rewriteSearch = new NaiveSearch()
    val initialState = new RewriteSearchState(hyperGraph)
    val spaceSearch = new RewriteSearchSpace((rewriteRules ++ SpaceComplexRewriteRulesDB.rewriteRules ++ TimeComplexRewriteRulesDB.rewriteRules).toSeq, initialState, _ => false)
    val (_, newState) = rewriteSearch.search(spaceSearch, 3)
    newState.graph
  }

  private def findEquive(populated: RewriteSearchState.HyperGraph): Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    val populatedPrograms = Programs(populated)
    def contains(big: AnnotatedTree, small: AnnotatedTree): Boolean = (big == small) || big.subtrees.exists(contains(_, small))
    val nodeCreator = Stream.from(populated.nodes.map(_.id).max).map(HyperTermId).iterator
    val r = for ((idMaps, _) <- populated.findSubgraph[Int](hyperPattern1) if idMaps(2) != idMaps(3)) yield {
        val trueTC = idMaps(0)
        val functionNode = idMaps(1)
        //      val functionTC = idMaps(2)
        val fullTC = idMaps(3)
        val functionArguments = idMaps.filterKeys(_ >= 4).toList.sortBy(_._1).map(_._2)
        val ttt = functionArguments.map(functionArguments => (functionArguments, populatedPrograms.reconstruct(functionArguments).filter(_.root != Language.applyId)))
        val fullTimeComplexes = populatedPrograms.reconstructTimeComplex(fullTC).toSet

        fullTimeComplexes.flatMap {
          case ContainerComplexity(tc) =>
            logger.error("ContainerComplexity in fullTimeComplexes: " + tc)
            Set.empty
          case ConstantComplexity(_) =>
            Seq(
              HyperEdge(trueTC, HyperTermIdentifier(Language.timeComplexId), Seq(functionNode, fullTC), EmptyMetadata),
            )
          case AddComplexity(complexities) =>
            val nonConstants = complexities.collect { case ContainerComplexity(tree) => tree }.filter(_.root == timeComplexFunction)
            nonConstants match {
              case Nil =>
                logger.error("Empty nonConstants in AddComplexity: " + complexities)
                Set.empty
              case Seq(oneCall) =>
                val arguments = oneCall.subtrees

                logger.debug("arguments " + arguments)
                val filtered = ttt.zip(arguments).map(t => (t._1._1, t._1._2.filter(a => contains(a, t._2)))).filter(_._2.nonEmpty)
                logger.debug("filtered " + filtered)
                filtered match {
                  case filtered if filtered.count(_._2.exists(_.root == Language.consId)) == 1 =>
                    val one = filtered.find(_._2.exists(_.root == Language.consId)).get
                    logger.debug(one.toString)
                    val newTC = nodeCreator.next()
                    Seq(
                      HyperEdge(trueTC, HyperTermIdentifier(Language.timeComplexId), Seq(functionNode, newTC), EmptyMetadata),
                      HyperEdge(newTC, HyperTermIdentifier(Identifier("len")), Seq(one._1), EmptyMetadata)
                    )
                }
            }
        }
      }
    r.flatten
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val filledHyperGraph = findUntouchedTerms(state.programs.hyperGraph)

    val populated = populateEdges(filledHyperGraph, state.rewriteRules)

    val newEdges = findEquive(populated)

    val newHyperGraph = populated ++ newEdges -- (filledHyperGraph -- state.programs.hyperGraph)
    state.copy(programs = Programs(newHyperGraph))
  }
}
