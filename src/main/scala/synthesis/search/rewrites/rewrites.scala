package synthesis.search.rewrites

import structures._
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Identifier

package object rewrites {
  private[synthesis] type HyperPrefix = immutable.HyperGraph[Item[HyperTermId, Int], (Item[HyperTermIdentifier, Int], Boolean)]

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
