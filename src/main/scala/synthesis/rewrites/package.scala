package synthesis.rewrites

import structures.HyperGraphManyWithOrderToOneLike.HyperGraphPrefix
import structures._
import syntax.Identifier
import synthesis.{HyperTermId, HyperTermIdentifier}

package object rewrites {
  private[synthesis] type HyperPrefix = HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)]

  private[synthesis] def patternEdgeCreator(target: Item[HyperTermId, Int],
                                           edgeType: Item[HyperTermIdentifier, Int],
                                           sources: Seq[Item[HyperTermId, Int]]):
  HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
    HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](target, edgeType, sources, EmptyMetadata)
  }

  private[synthesis] def patternEdgeCreator(target: Item[HyperTermId, Int],
                                           edgeType: Identifier,
                                           sources: Seq[Item[HyperTermId, Int]]):
  HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
    patternEdgeCreator(target, Explicit[HyperTermIdentifier, Int](HyperTermIdentifier(edgeType)), sources)
  }

  private[synthesis] def patternEdgeCreator(target: HyperTermId,
                                           edgeType: Item[HyperTermIdentifier, Int],
                                           sources: Seq[Item[HyperTermId, Int]]):
  HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
    patternEdgeCreator(Explicit[HyperTermId, Int](target), edgeType, sources)
  }

  private[synthesis] def patternEdgeCreator(target: HyperTermId,
                                           edgeType: Identifier,
                                           sources: Seq[Item[HyperTermId, Int]]):
  HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
    patternEdgeCreator(Explicit[HyperTermId, Int](target), edgeType, sources)
  }

  private[synthesis] def toExplicit(id: HyperTermId): Explicit[HyperTermId, Int] = Explicit(id)
  private[synthesis] def toExplicit(id: HyperTermIdentifier): Explicit[HyperTermIdentifier, Int] = Explicit(id)
}
