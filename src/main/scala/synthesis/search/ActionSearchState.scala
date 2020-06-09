package synthesis.search

import structures.mutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, search}
import synthesis.search.ActionSearchState.GraphUpdater
import synthesis.search.rewrites.IRewriteRule

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/18/18
  */
class ActionSearchState(private val graph: ActionSearchState.HyperGraph, rules: Set[IRewriteRule]) extends State[ActionSearchState] {
  def this(programs: Programs, rewriteRules: Set[IRewriteRule]) = this(new search.ActionSearchState.HyperGraph(programs.queryGraph.edges), rewriteRules)

  val mutableRules = mutable.Set(rules.toSeq: _*)

  def rewriteRules: Set[IRewriteRule] = mutableRules.toSet

  override def deepCopy(): ActionSearchState = new ActionSearchState(graph.clone, rewriteRules)
  val programs: Programs = Programs(graph)

  def updateGraph(op: GraphUpdater): Unit = op(graph)

  def addRule(rule: IRewriteRule): Unit = mutableRules += rule

  def addRules(rules: Set[_ <: IRewriteRule]): Unit = mutableRules ++= rules

  def removeRule(rule: IRewriteRule): Unit = mutableRules -= rule
}

object ActionSearchState {
  type HyperGraph = CompactHyperGraph[HyperTermId, HyperTermIdentifier]
  type GraphUpdater = HyperGraph => Unit
}
