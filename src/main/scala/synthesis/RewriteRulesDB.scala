package synthesis

import com.typesafe.scalalogging.LazyLogging
import relentless.BasicSignature
import relentless.BasicSignature._
import relentless.rewriting.RewriteRule._
import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge, Metadata}
import syntax.AstSugar._
import syntax.{AstSugar, Identifier}
import synthesis.rewrites.{FlattenRewrite, RewriteRule, RewriteSearchState}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator

/**
  * @author tomer
  * @since 12/27/18
  */
trait RewriteRulesDB extends LazyLogging {
  protected def vars: Set[Identifier]

  protected def ruleTemplates: Set[Term]

  protected def metadata: Metadata

  lazy val rewriteRules: Set[Operator[RewriteSearchState]] = Set[Operator[RewriteSearchState]](FlattenRewrite) ++ ruleTemplates.flatMap(ruleTemplatesToRewriteRules)

  private def ruleTemplatesToRewriteRules(ruleTemplate: Term): Set[RewriteRule] = {
    // TODO: use destruct pattern
    def termToHyperPattern(term: Term): HyperPattern = {
      val hyperPatternEdges = Programs.destruct(term).edges.filterNot(edge => vars.contains(edge.edgeType.identifier)).map(edge => {
        HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
          ReferenceTerm(edge.target.id),
          ExplicitTerm(edge.edgeType),
          edge.sources.map(hti => ReferenceTerm[HyperTermId](hti.id)),
          EmptyMetadata
        )
      })
      HyperGraphManyWithOrderToOne(hyperPatternEdges.toSeq: _*)
    }

    def split(hyperGraph: HyperPattern): (HyperPattern, HyperPattern) = {
      val baseEdge = hyperGraph.edges.maxBy(_.target match { case ReferenceTerm(id) => id })
      val maxLeft = baseEdge.sources.head match {
        case ReferenceTerm(id) => id
      }
      val otherEdges = hyperGraph.edges - baseEdge
      val leftEdges = otherEdges.filter(_.target match { case ReferenceTerm(id) => id <= maxLeft })
      val rightEdges = otherEdges.filter(_.target match { case ReferenceTerm(id) => id >= maxLeft })
      (HyperGraphManyWithOrderToOne(leftEdges.toSeq: _*), HyperGraphManyWithOrderToOne(rightEdges.toSeq: _*))
    }

    ruleTemplate.root match {
      case AstSugar.`=` =>
        val leftTerm = ruleTemplate.subtrees.head
        val rightTerm = ruleTemplate.subtrees.last
        logger.debug(s"equal $leftTerm $rightTerm")
        val testPattern = termToHyperPattern(ruleTemplate)
        val (leftPattern, rightPattern) = split(testPattern)
        Set(
          new RewriteRule(leftPattern, rightPattern, (a, b) => metadata),
          new RewriteRule(rightPattern, leftPattern, (a, b) => metadata)
        )
      case relentless.rewriting.RewriteRule.`=>` =>
        val leftTerm = ruleTemplate.subtrees.head
        val rightTerm = ruleTemplate.subtrees.last
        logger.debug(s"directional rewrite $leftTerm $rightTerm")
        val testPattern = termToHyperPattern(ruleTemplate)
        val (conditions, destination) = split(testPattern)
        Set(new RewriteRule(conditions, destination, (a, b) => metadata))
      case _ =>
        logger.debug(s"unknown ${ruleTemplate.root}")
        Set()
    }
  }
}

class SimpleRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z, `x'`, xs).map(_.root)

  import BasicSignature._

  override protected val ruleTemplates: Set[Term] = Set(
    `⇒:`(tt, y) =:> id(y),
    `⇒:`(ff, y) =:> ff,
    ~tt =:= ff,
    ~ff =:= tt,
    (x /: ff) =:> id(x),
    (ff /: x) =:> id(x),
    id(id(x)) =:> id(x),

    (x =:= `x'`) =:= in(`x'`, `{}`(x)),
    (x elem (`x'` cons `xs'`)) =:= ((x =:= `x'`) | (x elem `xs'`)),
    ~(x =:= y) =:= `!=:=`(x, y),
    ~in(x, y) =:= not_in(x, y),
    not_in(x, xs) =:= set_disj(`{}`(x), xs),
    set_disj(xs, `{}`(x)) =:> not_in(x, xs),
    ~(x | y) =:= (~x & ~y),
    ~(x & y) =:= (~x | ~y),
    (set_disj(x, xs) & set_disj(y, xs)) =:= set_disj(set_union(x, y), xs),
    (set_disj(xs, x) & set_disj(xs, y)) =:= set_disj(xs, set_union(x, y)),
    (x elem xs) =:= in(x, elems(xs)),
    elems(`x'` cons `xs'`) =:= set_union(`{}`(`x'`), elems(`xs'`)), // <-- this one is somewhat superfluous?

    (y snoc x) =:= (y ++ (x cons nil)),
    nil ++ `xs'` =:> id(`xs'`),
    `xs'` ++ nil =:> id(`xs'`),
    x ++ (y ++ z) =:= (x ++ y) ++ z,
    (x cons xs) ++ `xs'` =:= (x cons (xs ++ `xs'`)),

    ((x < y) ||| tt) =:> (x ≤ y),
    (x ≤ y) ||> (min(x, y) =:> id(x)),
    (x ≤ y) ||> (min(y, x) =:> id(x)),
    //    min(x, y) =:> min(y,x),

    (x ≤ y) ||> (bounded_minus(x, y) =:> zero),

    (xs take zero) =:> nil,
    (xs take len(xs)) =:> xs,
    ((xs ++ `xs'`) take x) =:> ((xs take min(len(xs), x)) ++ (`xs'` take bounded_minus(x, len(xs)))),

    // merge range
    (range_exclude(x, y) ++ range_exclude(y, z)) =:> range_exclude(x, z),
    // exclude to include
    range_exclude(x, y + one) =:= range_include(x, y),
    // singleton range
    range_include(x, x) =:= (x cons nil),
    (in(z, range_exclude(x, y)) ||| tt) =:> ((x ≤ z) ||| (z < y))
  )

  override protected def metadata: Metadata = EmptyMetadata
}

object SimpleRewriteRulesDB {
  def apply(): SimpleRewriteRulesDB = new SimpleRewriteRulesDB()
}

object AssociativeRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z).map(_.root)

  override protected val ruleTemplates: Set[Term] = Set(
    (x & (y & z)) =:= ((x & y) & z),
    (x + (y + z)) =:= ((x + y) + z),
    (x * (y * z)) =:= ((x * y) * z)
  )

  override protected def metadata: Metadata = AssociativeMetadata

  case object AssociativeMetadata extends Metadata {
    override def toStr: String = "AssociativeMetadata"
  }

}

object ExistentialRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(xs, exist).map(_.root)

  override protected val ruleTemplates: Set[Term] = Set(
    xs =:> ((xs take exist) ++ (xs drop exist))
  )

  override protected def metadata: Metadata = ExistentialMetadata

  case object ExistentialMetadata extends Metadata {
    override def toStr: String = "ExistentialMetadata"
  }

}
