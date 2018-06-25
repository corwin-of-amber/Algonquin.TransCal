package relentless.rewriting

import relentless.matching.{Encoding, Trie, Valuation}
import relentless.rewriting.RewriteRule.CompiledRule
import syntax.AstSugar._
import syntax.Formula.O
import syntax.{Formula, Identifier, Scheme, Tree}
import com.typesafe.scalalogging.LazyLogging
import relentless.matching._
import scala.collection.mutable
import scala.collection.immutable

class RewriteRule(val src: Scheme.Template, val target: Scheme.Template, val ruleType: RewriteRule.Category) {
  var compiled: Option[CompiledRule] = None

  def proccess(w: BaseRewriteEdge[Int], trie: Trie[Int, BaseRewriteEdge[Int]])(implicit encoding: Encoding): List[BaseRewriteEdge[Int]] = {
    if (compiled.isEmpty)
      compiled = Some(new CompiledRule(this))
    compiled.get.proccess(w, trie)
  }
}

object RewriteRule {
  /** Dont know yet
    *
    * @param shards
    * @param conclusion The original rules target converted into a bundle
    * @param nHoles
    * @param enc
    */
  class CompiledRule private(shards: List[Bundle], conclusion: Bundle, val nHoles: Int,
                             origin: RewriteRule)(implicit enc: Encoding) extends LazyLogging {

    private def this(pattern: Bundle, conclusion: Bundle, origin: RewriteRule)(implicit enc: Encoding) =
      this(pattern.shuffles, conclusion, pattern.minValuationSize, origin)

    def this(origin: RewriteRule)(implicit enc: Encoding) = {
      this(enc.toBundles(origin.src.vars map (T(_)): _*)(origin.src.template.split(RewriteRule.||>) map (_.split(RewriteRule.|||))).bare,
        enc.toBundle(origin.target.vars map (T(_)): _*)(origin.target.template.split(RewriteRule.|||): _*),
        origin)
    }

    //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
    // For debuging this might be better: T((enc <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)
    // It i sbetter to just write tests
    private def fresh(wv: immutable.IndexedSeq[Int]): Int = enc.reserveIndex()

    private def sparseTargetLookup(sparsePattern: Seq[(Int, Int)], t: Trie[Int, BaseRewriteEdge[Int]]): Option[Int] =
      t.sparseLookup(sparsePattern).map(_ (1))

    // TODO: remove assumptions
    // existential should be last.
    // This actually introduces an assumption that either all src vars are used or that the used ones are first?
    private val parameterIndexes: List[Int] = {
      val src = origin.src.vars.toSet
      var last = origin.target.vars.length + 1
      for (v <- origin.target.vars) yield {
        if (src.contains(v))
          1 + origin.src.vars.indexOf(v)
        else {
          last -= 1
          last
        }
      }
    }

    def proccess(w: BaseRewriteEdge[Int], trie: Trie[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
      val matcher = new Match(trie)(enc)
      val res = for (s <- shards;
                     valuation <- matcher.matchLookupUnify_*(s.patterns, w, new Valuation(nHoles))) yield {
        //println(s"valuation = ${valuation mkString " "}")
        val add = conclude(valuation, trie)
        logger.trace(s"added new words using ${s.patterns.map(_.mkString(" ")) mkString (", ")}. words: ${add map (_ mkString " ") mkString ", "}")
        add
      }
      res.flatten
    }

    /** Return all rewrites after filling in all the holes.
      *
      * @param valuation
      * @param trie containing all available rewrites
      * @return
      */
    private def conclude(valuation: Valuation, trie: Trie[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
      assert(valuation.length >= nHoles)

      def valuation_(i: Int): Option[HyperTerm] = {
        if (i < valuation.length && valuation.isDefined(i)) valuation(i)
        else if (i < valuation.length) None
        // --- introduces an existential
        else Some(HyperTerm(enc --> new Identifier("ex?" + i, "variable", new Uid)))
      }

      // 0 1 4 -> -1 -2 -5 / 0 1 4 map Placeholder  | valuation(i)/0/existential
      val newSubterms: mutable.Map[Placeholder, Option[HyperTerm]] = mutable.Map.empty[Placeholder, Option[HyperTerm]] ++
        ((0 :: parameterIndexes) map (i => (Placeholder(i), valuation_(i))))

      // construct new hyperedges by replacing holes (undefined) with valuation elements and fresh hyperterms
      for (pattern <- conclusion.patterns.reverse) yield {
        val updatedPattern: Pattern = new Pattern(pattern map {
          case x: Placeholder => newSubterms.get(x).map(_.getOrElse(x)).getOrElse(x)
          case x => x
        })

        // Try to find target in trie, if no such target exists create a new hyperterm
        val res = updatedPattern(1) match {
          case ph: Placeholder =>
            val sparsePattern = (0, BaseHyperTerm.toInt(updatedPattern(0))) +: (for (i <- 2 until updatedPattern.length) yield (i, BaseHyperTerm.toInt(updatedPattern(i))))
            val wv1: HyperTerm = sparseTargetLookup(sparsePattern, trie).map(HyperTerm) getOrElse HyperTerm(fresh(updatedPattern.map(BaseHyperTerm.toInt)))
            newSubterms += ph -> Some(wv1)
            RewriteEdge(origin, updatedPattern.updated(1, wv1).map(BaseHyperTerm.toInt))
          case _ => RewriteEdge(origin, updatedPattern.map(BaseHyperTerm.toInt))
        }
        assert(res.forall(_ > 0))
        res
      }
    }
  }

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }
  type Category = Category.Value

  // TODO: make sure all literals are in encoding (for example elems, head, take)

  val `=>` = I("=>", "operator") // directional rewrite
  val ||| = I("|||", "operator") // parallel patterns or conclusions
  val ||> = I("||>", "operator")

  implicit class RuleOps(private val t: Term) extends AnyVal {
    def =:>(s: Term): Tree[Identifier] = T(`=>`)(t, s)

    def |||(s: Term): Tree[Identifier] = T(RewriteRule.|||)(t, s)

    def ||>(s: Term): Tree[Identifier] = T(RewriteRule.||>)(t, s)
  }

  Formula.INFIX ++= List(`=>` -> O("=>", 5), `|||` -> O("|||", 5))

  def apply(template: Scheme.Template, ruleType: RewriteRule.Category): List[RewriteRule] = {
    RewriteRule(template.template, template.vars map (T(_)), ruleType)
  }

  def apply(rule: Tree[Identifier], vars: List[Tree[Identifier]], ruleType: RewriteRule.Category): List[RewriteRule] = {
    def varsUsed(t: Term) = vars filter t.leaves.contains

    rule match {
      case eqn@T(`=>`, List(lhs, rhs)) =>
        val left_v = varsUsed(lhs) map (_.leaf)
        val right_v = varsUsed(rhs) map (_.leaf)
        List(new RewriteRule(new Scheme.Template(left_v, lhs), new Scheme.Template(right_v, rhs), ruleType))
      case eqn@T(`=`, List(lhs, rhs)) =>
        val left_v = varsUsed(lhs) map (_.leaf)
        val right_v = varsUsed(rhs) map (_.leaf)
        val (l, r) = (new Scheme.Template(left_v, lhs), new Scheme.Template(right_v, rhs))
        List(new RewriteRule(l, r, ruleType), new RewriteRule(r, l, ruleType))
      case other =>
        throw new RuntimeException(s"invalid syntax for rule: ${other toPretty}")
    }
  }
}