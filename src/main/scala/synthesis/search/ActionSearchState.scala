package synthesis.search

import structures.mutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, search}
import synthesis.search.ActionSearchState.GraphUpdater
import synthesis.search.rewrites.RewriteRule

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/18/18
  */
class ActionSearchState(private val graph: ActionSearchState.HyperGraph, rules: Set[RewriteRule]) extends State[ActionSearchState] {
  def this(programs: Programs, rewriteRules: Set[RewriteRule]) = this(new search.ActionSearchState.HyperGraph(programs.queryGraph.edges), rewriteRules)

  val mutableRules = mutable.Set(rules.toSeq: _*)

  def rewriteRules: Set[RewriteRule] = mutableRules.toSet

  override def deepCopy(): ActionSearchState = new ActionSearchState(graph.clone, rewriteRules)
  val programs: Programs = Programs(graph)

  def updateGraph(op: GraphUpdater): Unit = op(graph)

  def addRule(rule: RewriteRule): Unit = mutableRules += rule

  def addRules(rules: Set[_ <: RewriteRule]): Unit = mutableRules ++= rules

  def removeRule(rule: RewriteRule): Unit = mutableRules -= rule
}

object ActionSearchState {
  type HyperGraph = CompactHyperGraph[HyperTermId, HyperTermIdentifier]
  type GraphUpdater = HyperGraph => Unit
}
