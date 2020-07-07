package synthesis.search

import org.scalatest.{Matchers, PropSpec}
import synthesis.Programs
import synthesis.search.ActionSearchState.HyperGraph
import synthesis.search.DepthAwareSearch.DepthMetadata
import synthesis.search.actions.LetAction
import synthesis.search.rewrites.{PatternRewriteRule, RewriteRule}
import transcallang.TranscalParser

class DepthAwareSearchTest extends PropSpec with Matchers {
  val parser = new TranscalParser()
  val rewrite: Set[RewriteRule] = new LetAction(parser apply "?x >> x + ?y").rules.map(_.asInstanceOf[RewriteRule])
  val graph: HyperGraph = new ActionSearchState.HyperGraph(Programs(parser.parseExpression("x")).queryGraph.edges)

  property("Appliying rules to depth increases depth") {
    val state = new ActionSearchState(graph.clone, rewrite)
    val newState = new DepthAwareSearch(3).apply(state, 5)
    val depths = newState.programs.queryGraph.edges.flatMap(_.metadata.map({
      case DepthMetadata(x) => x
      case _ => 0
    }))
    depths.max shouldEqual 3
  }

  property("Edges should be filtered by depth") {
    val state = new ActionSearchState(graph.clone, rewrite)
    val newState = new DepthAwareSearch(2).apply(state, 4)
    newState.programs.queryGraph.edges.size should be < 50
  }

  property("Edges should only contain one depthmetadata") {
    val state = new ActionSearchState(graph.clone, rewrite)
    val newState = new DepthAwareSearch(2).apply(state, 4)
    newState.programs.queryGraph.edges.exists(_.metadata.collect({
      case a @ DepthMetadata(x) => a
    }).size > 1) shouldEqual false
  }
}
