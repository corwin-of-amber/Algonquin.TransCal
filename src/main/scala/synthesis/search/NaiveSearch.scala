package synthesis.search

import com.typesafe.scalalogging.LazyLogging
import structures.HyperEdge
import structures.generic.HyperGraph
import synthesis.search.actions.SearchAction
import synthesis.search.rewrites.Template.TemplateTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

/**
  * BFS returns last state only.
  */
class NaiveSearch(startVersioned: Boolean = false, isGoal: ActionSearchState.HyperGraph => Boolean)
  extends SearchAction with LazyLogging {

  protected def postProcessors: Set[Set[HyperEdge[HyperTermId, HyperTermIdentifier]] => Set[HyperEdge[HyperTermId, HyperTermIdentifier]]] = Set.empty

  /* --- Search Impl. --- */
  override def apply(state: ActionSearchState, depth: Double): ActionSearchState = {
    state.updateGraph(graph => {
      logger.debug(s"Starting Naive Search. Graph size: ${graph.size}")

      var i = 0

      // As we always have same operators I shortcut.
      // Using stepping API we can parallelize our work.
      val operators = state.rewriteRules.par

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
          //        "?x :: ?xs ||| ?y :+ ?y"
        ).map(s => (s, parser.parseExpression(s) cleanTypes)).map({ case (s, t) => (s, Programs.destructPatternWithRoot(t)) })
      }


      var prevGraph: Option[ActionSearchState.HyperGraph] = None
      while (i < depth && !isGoal(graph) && !prevGraph.contains(graph)) {
        val versioned = prevGraph.isDefined || startVersioned
        prevGraph = Some(graph.clone)
        for ((term, (pattern, patternRoot)) <- patterns) {
          val reconstructed = state.programs.reconstructPatternWithRoot(pattern, patternRoot)
          if (reconstructed.nonEmpty) {
            logger.info(term)
            for ((id, rTerms) <- reconstructed) {
              logger.info(s"$id: ${rTerms.toList.map(Programs.termToString).mkString("  ---  ")}")
            }
          }
        }

        val hyperTermIds: Seq[() => HyperTermId] = {
          val graphEmpty = graph.isEmpty
          val maxId = graph.nodes.map(_.id).max
          0 until operators.size map (j => {
            val creator =
              Stream.from(if (graphEmpty) j else maxId + 1 + j, operators.size)
                .map(HyperTermId).iterator
            () => creator.next
          })
        }

        // TODO: fix after fixing rewrite rule traits
        val steps = operators.map(r => r.getStep(graph, versioned))
        val newEdges = steps.zip(hyperTermIds).map({ case (es, idCreator) => structures.generic.HyperGraph.fillWithNewHoles(es, idCreator) }).seq
        val afterUpdate = postProcessors.foldLeft(newEdges.flatten)((es, f) => f(es))
        graph ++= afterUpdate
        i += 1

        logger.info(s"Done a round robin. Graph size is: ${graph.size}")
      }
    })

    state
  }
}


