package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import structures.HyperEdge
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.rewrites.{RewriteRule, RewriteSearchSpace, RewriteSearchState}
import synthesis.rewrites.Template.TemplateTerm
import transcallang.AnnotatedTree

import scala.collection.mutable

/**
  * BFS returns last state only.
  */
class NaiveSearch extends SearchDepth[RewriteSearchState, RewriteSearchSpace, RewriteSearchState] with LazyLogging {

  /* --- Search Impl. --- */

  def search(searchSpace: RewriteSearchSpace, maxDepth: Double): (Boolean, RewriteSearchState) = {
    val state = searchSpace.initialStates.head
    state match {
      case state1: RewriteSearchState => logger.debug(s"Starting Naive Search. Graph size: ${state1.graph.size}")
      case _ =>
    }
    val operatorVer: mutable.Map[Operator[RewriteSearchState], Long] = mutable.Map.empty
    var i = 0

    // As we always have same operators I shortcut.
    // Using stepping API we can parallelize our work.
    val operators = searchSpace.operators(state).collect({
      case o: StepOperator[Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]], RewriteSearchState] => o
    }).par
    assert(operators.size == searchSpace.operators(state).size)
    // Don't need to check for new nodes when using versioning. Versioning finally faster.
    var prevVersion = 0L

    // Adding hardcoded patterns for debuging
    val patterns = {
      import transcallang.TranscalParser
      val parser = new TranscalParser
      Seq[String](
//        "filter ?p (?x :: ?xs)",
//        "x::nil",
//        "y::x::nil",
//        "reverse(snoc(nil, ?z))",
//        "reverse(snoc(x::nil, ?z))",
//        "reverse(snoc(y::x::nil, ?z))"
      ).map(s => (s, parser.parseExpression(s).map(_.copy(annotation=None)))).map({case (s, t) => (s, Programs.destructPatternWithRoot(t))})
    }


    var prevState: Option[RewriteSearchState] = None
    while (i < maxDepth && !searchSpace.isGoal(state) && !prevState.contains(state)) {
      prevState = Some(state.deepCopy())
      for ((term, (pattern, patternRoot)) <- patterns) {
        val reconstructed = Programs.reconstructPatternWithRoot(state.graph, pattern, patternRoot)
        if (reconstructed.nonEmpty) {
          logger.info(term)
          for ((id, rTerms) <- reconstructed) {
            logger.info(s"$id: ${rTerms.toList.map(Programs.termToString).mkString("  ---  ")}")
          }
        }
      }
      val nextVersion = state.graph.version
      val hyperTermIds: Seq[() => HyperTermId] = 0 until operators.size map(j => {
        val creator =
          Stream.from(if (state.graph.isEmpty) j else state.graph.nodes.map(_.id).max + 1 + j, operators.size)
            .map(HyperTermId).iterator
        () => creator.next
      })

      val steps = operators.map(r => r.getStep(state, prevVersion))
      val newEdges = steps.zip(hyperTermIds).map({case (es, idCreator) => structures.generic.HyperGraph.fillWithNewHoles(es, idCreator)}).seq
      state.graph ++= newEdges.flatten
      prevVersion = nextVersion
      i += 1

      logger.info(s"Done a round robin. Graph size is: ${state.graph.size}")
    }

    (searchSpace.isGoal(state), state)
  }
}


