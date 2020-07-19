package synthesis.search.actions.thesy

import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import report.{LazyTiming, StopWatch}
import structures.HyperEdge
import structures.mutable.CompactHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, search}
import synthesis.search.actions.{Action, ObservationalEquivalence, SearchAction}
import synthesis.search.rewrites.RewriteRule
import synthesis.search.rewrites.RewriteRule.HyperGraph
import synthesis.search.{ActionSearchState, Operator, rewrites}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.collection.mutable

class ConjectureChecker(prover: Prover, searcher: SearchAction, maxDepth: Double) extends LazyLogging with LazyTiming {
  //override protected def watchName: String = "ConjectureChecker"

  private val allfailed: mutable.Set[(AnnotatedTree, AnnotatedTree)] = mutable.Set.empty
  private implicit val smallestGeneraliestOrdering: Ordering[AnnotatedTree] = new Ordering[AnnotatedTree] {
    def getphs(annotatedTree: AnnotatedTree) = annotatedTree.nodes.map(_.root.literal)
      .filter(_.toLowerCase.contains(placeholderString.toLowerCase())).toSet.size

    override def compare(x: AnnotatedTree, y: AnnotatedTree): Int =
      if (x.size < y.size) -1
      else if (x.size > y.size) 1
      else if (getphs(x) > getphs(y)) -1
      else if (getphs(x) < getphs(y)) 1
      else 0
  }

  private val foundRules = mutable.Set.empty[RewriteRule]
  private val failedAttempts = mutable.Buffer.empty[(AnnotatedTree, AnnotatedTree)]
  private var retriedProofs = 0
  private val oe = new ObservationalEquivalence(searcher)

  protected def retryFailed: Set[(AnnotatedTree, AnnotatedTree)] = {
    var i = 0
    val found = mutable.Buffer.empty[(AnnotatedTree, AnnotatedTree)]
    logger.info("Retrying failed proofs")
    while (i < failedAttempts.length) {
      val (term1, term2) = failedAttempts.head

      val eqs = oe.fromTerms(Seq(term1, term2), prover.knownRules, maxDepth)
      if (eqs.size == 1) {
        failedAttempts.remove(i)
      } else {
        val res = prover.inductionProof(term1, term2) //.collect({ case rr => rr })
        if (res.nonEmpty) {
          foundRules ++= res
          found.append((term1.map(i => if (i.literal.startsWith(placeholderString)) i.copy(literal = "?" + i.literal) else i),
            term2.map(i => if (i.literal.startsWith(placeholderString)) i.copy(literal = "?" + i.literal) else i)))
          failedAttempts.remove(i)
          retriedProofs += 1
          i = 0
        } else i += 1
      }
    }
    logger.info("Done Retrying failed proofs")
    found.toSet
  }

  val checkCache: mutable.Map[AnnotatedTree, (HyperTermIdentifier, rewrites.RewriteRule.HyperGraph)] =
    mutable.Map.empty[AnnotatedTree, (HyperTermIdentifier, rewrites.RewriteRule.HyperGraph)]

  /** This includes both the conjecture screening and the proving as they are highly coupled.
    *
    * @param conjectures equality classes from soe
    * @return All proved rules
    */
  def checkConjectures(conjectures: Set[Set[AnnotatedTree]]): Set[AnnotatedTree] = timed {
    // First order the equality classes by smallest most general representative
    conjectures.toSeq.sortBy(_.min).collect({
      case terms if terms.size > 1 =>
        // refine equivalence relation using searcher
        logger.info("Filtering terms that don't need induction using observational equivalence")
        val graphs = terms.map(t => {
          if (!checkCache.contains(t)) {
            checkCache(t) = {
              val anchor = Identifier(UUID.randomUUID().toString)
              (HyperTermIdentifier(anchor), Programs.destruct(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId,
                List(t, AnnotatedTree.identifierOnly(anchor)))))
            }
          }
          checkCache(t)._2
        }).toSet

//        assert(graphs.toSeq.combinations(2).forall({ x: Seq[HyperGraph] => x.head.edges != x.last.edges }))

        def moveEdge(n: Int, hyperEdge: HyperEdge[HyperTermId, HyperTermIdentifier]) =
          hyperEdge.copy(target = hyperEdge.target.copy(hyperEdge.target.id + n), sources = hyperEdge.sources.map(s => s.copy(s.id + n)))

        val finerEquives = {
          val updatedGraph = graphs.reduce[HyperGraph]({ case (g1, g2) => g1 ++= g2.map(moveEdge(g1.nodes.map(_.id).max, _)) })
          terms.foreach(t => checkCache(t) = (checkCache(t)._1, updatedGraph))
          val state = oe(new search.ActionSearchState(updatedGraph, prover.knownRules), Some(maxDepth))
          val mapping = terms.groupBy(t => state.programs.queryGraph.findByEdgeType(checkCache(t)._1).head.target)
          mapping.values.toSet
        }

        // Take smallest of each set as heuristic to maximize knowledge gained from each theorem
        val representatives = finerEquives.map(_.minBy(_.size)).toSeq
        representatives.combinations(2).toSeq.flatMap(it => {
          val (term1, term2) = (it(0), it(1))
          // TODO: should go through all known types as induction var
          val res = prover.inductionProof(term1, term2).collect({ case rr => rr })
          if (res.nonEmpty) {
            foundRules ++= res
            logger.warn(s"Retrying failed depth @  ${StopWatch.instance.now}")
            // TODO: would probably be better to hold a priority queue and control it like this
            val newOnes = retryFailed
            logger.warn(s"Finished refailed depth @  ${StopWatch.instance.now}")
            if (allfailed.contains((term1, term2)))
              retriedProofs += 1
            newOnes ++ Some((term1.map(i => if (i.literal.startsWith(placeholderString)) i.copy(literal = "?" + i.literal) else i),
              term2.map(i => if (i.literal.startsWith(placeholderString)) i.copy(literal = "?" + i.literal) else i)))
          } else {
            failedAttempts.append((term1, term2))
            allfailed.add((term1, term2))
            Set.empty[(AnnotatedTree, AnnotatedTree)]
          }
        })
    }

    ).toSet flatMap ((trees: Seq[(AnnotatedTree, AnnotatedTree)]) => trees.map({
      case (t1, t2) => AnnotatedTree.withoutAnnotations(Language.letId, Seq(t1, t2))
    }))
  }

  def prettify(term: AnnotatedTree) = {
    val phs = term.nodes.map(_.root).filter(_.literal.startsWith("?"))
    term.map((v: Identifier) => phs.indexOf(v) match {
      case i if i >= 0 => new Identifier(s"?${
        i
      }")
      case _ => v
    })
  }

  def stringForRule(lhs: AnnotatedTree, rhs: AnnotatedTree) =
    Programs.termToString(prettify(AnnotatedTree(transcallang.Language.letId, Seq(lhs, rhs), Seq())))
}
