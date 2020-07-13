package synthesis.search.actions.thesy

import com.typesafe.scalalogging.LazyLogging
import report.LazyTiming
import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.actions.{Action, LetAction, SearchAction}
import synthesis.search.rewrites.{PatternRewriteRule, RewriteRule}
import transcallang._

import scala.collection.mutable

class Prover(datatypes: Set[Datatype], searcher: SearchAction, rules: Set[RewriteRule])
    extends LazyLogging with LazyTiming {
  import Prover._

  private var failedProofs = 0
  private val mutableRules = mutable.Set.empty ++= rules
  private val foundRules = mutable.Set.empty[AnnotatedTree]
  private val ltwfId = Identifier("ltwf")
  private val ltwfTransivity = new LetAction(new TranscalParser()("ltwf(?x, ?y) ||| ltwf(?z, x) >> ltwf(z, y)")).rules

  // TODO: bad design change this
  def knownRulesTrees: Set[AnnotatedTree] = foundRules.toSet
  def knownRules: Set[RewriteRule] = mutableRules.toSet

  private def ltwfRules(datatype: Datatype): Set[PatternRewriteRule] = {
    val contructorRules = datatype.constructors.flatMap({ c =>
      val holesAndIgnores = c.annotation.get.subtrees.dropRight(1).zipWithIndex.map({
        // TODO: Once we support dependant types or enumerationg polymorphic types instantiantions we need to change this
        case (t, i) if t == datatype.asType => AnnotatedTree.identifierOnly(Identifier(s"?param$i"))
        case _ => AnnotatedTree.identifierOnly(Identifier("_"))
      })
      val rootTree = AnnotatedTree.identifierOnly(Identifier("?root"))
      val lhs = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, Seq(
        rootTree,
        AnnotatedTree.withoutAnnotations(c.copy(annotation = None), holesAndIgnores)
      ))

      val trueTree = AnnotatedTree.identifierOnly(Language.trueId)
      val holesLtwf = AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, trueTree +: holesAndIgnores.collect({
        case holeTree if holeTree.root.literal.startsWith("?") =>
          AnnotatedTree.withoutAnnotations(ltwfId, Seq(holeTree, rootTree))
      }))

      new LetAction(AnnotatedTree.withoutAnnotations(Language.limitedDirectedLetId, Seq(
        lhs,
        holesLtwf
      ))).rules
    })
    ltwfTransivity ++ contructorRules
  }

  private def createHypothesis(term1: AnnotatedTree,
                               term2: AnnotatedTree,
                               inductionPh: Identifier): Seq[PatternRewriteRule] = {
    // TODO: why is somevar name needed?
    val hypothIndcVar = "?SomeVar"
    val (cleanTerm1, cleanTerm2) = (term1.map(identMapper(_, inductionPh, hypothIndcVar)),
      term2.map(identMapper(_, inductionPh, hypothIndcVar)))

    val precondition = AnnotatedTree.withoutAnnotations(ltwfId,
      Seq(Identifier("?SomeVar"), cleanVars(identMapper(inductionPh, inductionPh))).map(AnnotatedTree.identifierOnly))

    // Precondition on each direction of the hypothesis
    Seq((cleanTerm1, cleanTerm2), (cleanTerm2, cleanTerm1)).flatMap({ case (t1, t2) =>
      createRules(
        AnnotatedTree.withoutAnnotations(Language.trueCondBuilderId, Seq(precondition, t1)), t2)
    })
  }

  def inductionProof(tree1: AnnotatedTree, tree2: AnnotatedTree): Set[_ <: RewriteRule] = timed {
    // Each placeholder represents a value of a type.
    // To deal with multi param expressions some of the placeholders were duplicated ahead of time, so now just use 'em

    val term1 = tree1.cleanTypes
    val term2 = tree2.cleanTypes

    // TODO: Once we support dependant types or enumerationg polymorphic types instantiantions we need to change this
    // TODO: Try all knwon types
    val relevantTypes = datatypes.filter(d => tree1.nodes.map(_.root.annotation).contains(Some(d.asType))
      || tree2.nodes.map(_.root.annotation).contains(Some(d.asType)))

    if (relevantTypes.isEmpty) return Set.empty

    assert(relevantTypes.size == 1)
    val ourType = relevantTypes.head

    // Create new rewrite rule for induction hypothesis
    // Need to demand that the used hypothesis works only on structures smaller than new applied constructor
    // For that i created the ltwf relation. The constructor rules will add the needed intial relations.
    // So I will do directed let and add ltwf(?params from correct type, PH0) on the premise
    val inductionPh = inductionVar(ourType.asType).copy(annotation = None)
    if ((!term1.nodes.map(_.root).contains(inductionPh)) || (!term2.nodes.map(_.root).contains(inductionPh)))
      return Set.empty

    logger.info(s"Trying to prove ${Programs.termToString(term1)} = ${Programs.termToString(term2)}")

    val updatedTerm1 = term1.map(identMapper(_, inductionPh))
    val updatedTerm2 = term2.map(identMapper(_, inductionPh))

    val hypoths = createHypothesis(term1, term2, inductionPh)

    val conses = ourType.constructors.filter(_.annotation.exists(_.root == Language.mapTypeId))
    // TODO: move forall logic out of if condition
    if (conses.forall({ c =>
      // Replace inductionPh by inductionVar
      // Create base graph where inductionPh ||| c(existentials: _*)
      val constructedVal = AnnotatedTree.withoutAnnotations(c.copy(annotation = None),
        c.annotation.get.subtrees.dropRight(1).zipWithIndex.map({ case (_, i) =>
          AnnotatedTree.identifierOnly(Identifier(s"param$i"))
        })
      )
      val phToConstructed = createRules(
        AnnotatedTree.identifierOnly(cleanVars(identMapper(inductionPh, inductionPh))),
        constructedVal
      )

      val cleanUpdatedTerms = Seq(updatedTerm1, updatedTerm2).map(_.map(cleanVars))
      val state = new ActionSearchState(Programs(cleanUpdatedTerms.head).addTerm(cleanUpdatedTerms.last), knownRules ++ hypoths ++ phToConstructed ++ ltwfRules(ourType))
      val nextState = searcher(state)
      val pattern = Programs.destructPattern(AnnotatedTree.withoutAnnotations(Language.andCondBuilderId, cleanUpdatedTerms))
      nextState.programs.queryGraph.findSubgraph[Int](pattern).nonEmpty
    })) {
      logger.info(s"Found inductive rule: ${Programs.termToString(updatedTerm1)} = ${Programs.termToString(updatedTerm2)}")
      val newRules = createRules(updatedTerm1, updatedTerm2, save = true) ++ createRules(updatedTerm2, updatedTerm1, save = true)
      mutableRules ++= newRules
      newRules
    } else {
      logger.info(s"Proof Failed")
      failedProofs += 1
      Set.empty
    }
  }

  def createRules(lhs: AnnotatedTree, rhs: AnnotatedTree, save: Boolean = false) = {
    val term = AnnotatedTree.withoutAnnotations(Language.directedLetId, Seq(lhs, rhs))
    if (save) foundRules += term
    new LetAction(term,
      allowExistential = false).rules
  }
}

object Prover {
  private def identMapper(i: Identifier, inductionPh: Identifier, inductionVarName: String = "?inductionVar") =
    i match {
      // TODO: remote special case for induction var
      case i if i == inductionPh => i.copy(literal = inductionVarName)
      case i if i.literal.startsWith(placeholderString) => i.copy(literal = "?" + i.literal)
      case i => i
    }

  private def cleanVars(i: Identifier): Identifier =
    if (i.literal.startsWith("?")) i.copy(literal = i.literal.drop(1)) else i
}
