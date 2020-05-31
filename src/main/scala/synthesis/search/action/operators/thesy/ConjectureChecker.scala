package synthesis.search.action.operators.thesy

import com.typesafe.scalalogging.LazyLogging
import report.StopWatch
import synthesis.Programs
import synthesis.search.{Operator, action}
import synthesis.search.action.ActionSearchState
import synthesis.search.action.operators.{CaseSplitAction, LocateAction, ObservationalEquivalence, SearchAction}
import synthesis.search.rewrite.RewriteSearchState
import transcallang.{AnnotatedTree, Language}

import scala.collection.mutable

class ConjectureChecker(prover: Prover, searcher: SearchAction) extends LazyLogging {

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

  private val foundRules = mutable.Set.empty[Operator[RewriteSearchState]]
  private val failedAttempts = mutable.Buffer.empty[(AnnotatedTree, AnnotatedTree)]
  private var retriedProofs = 0

  protected def retryFailed: Set[(AnnotatedTree, AnnotatedTree)] = {
    var i = 0
    val found = mutable.Buffer.empty[(AnnotatedTree, AnnotatedTree)]
    logger.info("Retrying failed proofs")
    while (i < failedAttempts.length) {
      val (term1, term2) = failedAttempts.head

      val newState = searcher(new ActionSearchState(Programs(term1).addTerm(term2), prover.knownRules))
      if (newState.programs.findTree(term1).intersect(newState.programs.findTree(term2)).nonEmpty) {
        failedAttempts.remove(i)
      } else {
        val res = prover.inductionProof(term1, term2).collect({ case rr => rr })
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

  /** This includes both the conjecture screening and the proving as they are highly coupled.
    *
    * @param conjectures equality classes from soe
    * @return All proved rules
    */
  def checkConjectures(conjectures: Set[Set[AnnotatedTree]]): Set[AnnotatedTree] = {
    // First order the equality classes by smallest most general representative
    conjectures.toSeq.sortBy(_.min).collect({
      case terms if terms.size > 1 =>
        // refine equivalence relation using searcher
        logger.info("Filtering terms that don't need induction using observational equivalence")
        val finerEquives = new action.operators.ObservationalEquivalence(searchAction = Some(searcher)).fromTerms(terms.toSeq, prover.knownRules)
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
    }).toSet flatMap ((trees: Seq[(AnnotatedTree, AnnotatedTree)]) => trees.map({case (t1,t2) => AnnotatedTree.withoutAnnotations(Language.letId, Seq(t1, t2))}))
  }
}
