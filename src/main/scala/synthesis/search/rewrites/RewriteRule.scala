package synthesis.search.rewrites

import structures.HyperGraphLike.HyperEdgePattern
import structures.generic.HyperGraph
import structures.generic.HyperGraph.Match
import structures.{EmptyMetadata, HyperEdge, Metadata, generic, mutable}
import synthesis.search.rewrites.PatternRewriteRule.CategoryMetadata.Value
import synthesis.search.rewrites.PatternRewriteRule.MutableHyperPattern
import synthesis.search.rewrites.Template.TemplateTerm
import synthesis.{HyperTermId, HyperTermIdentifier}

trait RewriteRule {
  // TODO: This doesn't have to be versioned, it is as we don't change implementation during refactoring
  def apply(graph: RewriteRule.HyperGraph): Unit

  def applyVersioned(graph: RewriteRule.HyperGraph): Unit

  def isExistential: Boolean

  def getStep(graph: RewriteRule.HyperGraph, versioned: Boolean): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]]

  protected val creators = collection.mutable.Set.empty[Match[HyperTermId, HyperTermIdentifier, Int] => Metadata]

  def registerMetadataCreator(creator: Match[HyperTermId, HyperTermIdentifier, Int] => Metadata): this.type = {
    creators += creator
    this
  }

  def unregisterMetadataCreator(creator: Match[HyperTermId, HyperTermIdentifier, Int] => Metadata): this.type  = {
    creators -= creator
    this
  }

  protected val processors: collection.mutable.Buffer[(HyperGraph.Match[HyperTermId, HyperTermIdentifier, Int], MutableHyperPattern) => MutableHyperPattern] = collection.mutable.Buffer.empty

  def registerPostprocessor(processor: (HyperGraph.Match[HyperTermId, HyperTermIdentifier, Int], MutableHyperPattern) => MutableHyperPattern): this.type = {
    processors += processor
    this
  }

  def unregisterPostprocessor(processor: (HyperGraph.Match[HyperTermId, HyperTermIdentifier, Int], MutableHyperPattern) => MutableHyperPattern): this.type = {
    processors -= processor
    this
  }
}

object RewriteRule {
  /* --- Public --- */
  type HyperPattern = generic.HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int]
  type MutableHyperPattern = mutable.HyperGraph[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  type HyperPatternEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  // TODO: no reason this shouldn't be versioned
  type HyperGraph = structures.mutable.CompactHyperGraph[HyperTermId, HyperTermIdentifier]

  case class RewriteRuleMetadata(origin: PatternRewriteRule, originalEdges: PatternRewriteRule.HyperPattern) extends Metadata {
    override def toStr: String = s"RewriteRuleMetadata($origin, $originalEdges)"
  }

  object CategoryMetadata extends Enumeration with Metadata {
    val Basic, Associative, Goal, Definition, Existential = Value

    override protected def toStr: String = this.getClass.getName
  }

  def fillPatterns(hyperGraph: generic.HyperGraph[HyperTermId, HyperTermIdentifier], patterns: Seq[HyperPattern]): Iterator[Seq[Set[HyperEdge[HyperTermId, HyperTermIdentifier]]]] = {
    patterns match {
      case Nil => Iterator(Seq.empty)
      case pattern :: rest =>
        hyperGraph.findSubgraph[Int](pattern).iterator.flatMap {
          maps =>
            val fullPattern = generic.HyperGraph.fillPattern(pattern, maps, () => throw new RuntimeException("unknown reason"))
            fillPatterns(hyperGraph, rest.map(generic.HyperGraph.mergeMatch(_, maps)))
              .map(a => fullPattern +: a)
        }
    }
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = generic.HyperGraph(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters, EmptyMetadata)).toSeq: _*
  )

  /* --- Privates --- */
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}