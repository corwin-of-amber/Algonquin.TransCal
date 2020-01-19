package structures

import org.scalacheck.Gen
import org.scalacheck.Gen._
import synthesis.{HyperTermId, HyperTermIdentifier}

import scala.util.Random

package object mutable {
  def HyperEdgeGenFactory[Node, Edge](nodeSource: Gen[Node], edgeSource: Gen[Edge]): Gen[HyperEdge[Node, Edge]] = structures.HyperEdgeGenFactory(nodeSource, edgeSource)
  val integerEdgesGen: Gen[HyperEdge[Int, Int]] = structures.integerEdgesGen
  def grapher[Node, Edge](se: Set[HyperEdge[Node, Edge]]): VocabularyHyperGraph[Node, Edge] = VocabularyHyperGraph(se.toSeq:_*)

  def HyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[VocabularyHyperGraph[Node, Edge]] = {
    containerOf[Set, HyperEdge[Node, Edge]](edgeSource) map grapher
  }

  val integerGraphGen: Gen[VocabularyHyperGraph[Int, Int]] = HyperGraphGenFactory(integerEdgesGen)
  val versionedIntegerGraphGen: Gen[VersionedHyperGraph[Int, Int]] = integerGraphGen.map(new VersionedHyperGraph(_))
  val compactIntegerGraphGen: Gen[CompactHyperGraph[Int, Int]] = containerOf[Set, HyperEdge[Int, Int]](integerEdgesGen).map {e => new CompactHyperGraph(e)}
  val compactHyperGraphGen: Gen[CompactHyperGraph[HyperTermId, HyperTermIdentifier]] =
    containerOf[Set, HyperEdge[HyperTermId, HyperTermIdentifier]](edgeCreator).map { e => new CompactHyperGraph(e)}

  def WordGenFactory[Letter](letterSource: Gen[Letter]): Gen[Seq[Letter]] = choose( 1, 7).flatMap(containerOfN[Seq, Letter](_, letterSource))

  val integerLetterGen: Gen[Int] = oneOf(0 to 20)
  val integerWordGen: Gen[Seq[Int]] = WordGenFactory(integerLetterGen)

  def TrieGenFactory[Letter](wordSource: Gen[Seq[Letter]]): Gen[Trie[Letter]] = {
    containerOf[Set, Seq[Letter]](wordSource) map Trie[Letter]
  }

  val integerTrieGen: Gen[Trie[Int]] = TrieGenFactory(integerWordGen)
}
