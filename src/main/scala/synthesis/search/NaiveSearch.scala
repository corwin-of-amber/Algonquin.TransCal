package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import structures.HyperEdge
import synthesis.search.rewrites.Template.TemplateTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/**
  * BFS returns last state only.
  */
class NaiveSearch(startVersioned: Boolean = false) extends SearchDepth[RewriteSearchState, RewriteSearchSpace, RewriteSearchState] with LazyLogging {

  /* --- Search Impl. --- */

  def search(searchSpace: RewriteSearchSpace, maxDepth: Double): (Boolean, RewriteSearchState) = {
    val state = searchSpace.initialStates.head
    state match {
      case state1: RewriteSearchState => logger.debug(s"Starting Naive Search. Graph size: ${state1.graph.size}")
      case _ =>
    }
    var i = 0

    // As we always have same operators I shortcut.
    // Using stepping API we can parallelize our work.
    val operators = searchSpace.operators(state).collect({
      case o: StepOperator[Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]], RewriteSearchState] => o
    }).par
    assert(operators.size == searchSpace.operators(state).size)

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
      ).map(s => (s, parser.parseExpression(s) cleanTypes)).map({ case (s, t) => (s, Programs.destructPatternWithRoot(t)) })
    }


    var prevState: Option[RewriteSearchState] = None
    while (i < maxDepth && !searchSpace.isGoal(state) && !prevState.contains(state)) {
      val versioned = prevState.isDefined || startVersioned
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

      val hyperTermIds: Seq[() => HyperTermId] = {
        val graphEmpty = state.graph.isEmpty
        val maxId = state.graph.nodes.map(_.id).max
        0 until operators.size map (j => {
          val creator =
            Stream.from(if (graphEmpty) j else maxId + 1 + j, operators.size)
              .map(HyperTermId).iterator
          () => creator.next
        })
      }

      val steps = operators.map(r => r.getStep(state, versioned))
      val newEdges = steps.zip(hyperTermIds).map({ case (es, idCreator) => structures.generic.HyperGraph.fillWithNewHoles(es, idCreator) }).seq
      state.graph ++= newEdges.flatten
      i += 1

      logger.info(s"Done a round robin. Graph size is: ${state.graph.size}")
    }

    (searchSpace.isGoal(state), state)
  }
}


