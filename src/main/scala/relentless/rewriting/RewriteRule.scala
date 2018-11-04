package relentless.rewriting

import java.io.Writer

import com.typesafe.scalalogging.LazyLogging
import relentless.Utils
import relentless.matching.{Encoding, _}
import relentless.matching.structures.filling._
import relentless.matching.structures.vocabulary.Vocabulary
import relentless.rewriting.RewriteRule.CompiledRule
import syntax.AstSugar._
import syntax.Formula.O
import syntax.{Formula, Identifier, Scheme, Tree}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer



class RewriteRule(val src: Scheme.Template, val target: Scheme.Template, val precond: Seq[Scheme.Template], val ruleType: RewriteRule.Category) extends LazyLogging {

  def this(src: Scheme.Template, target: Scheme.Template, ruleType: RewriteRule.Category) =
    this(src, target, Seq(), ruleType)

  private var compiled: Option[CompiledRule] = None

  /** The first bundle is translated normally with its root encoded as ~0.
    * The holes are encoded ~1 through ~holes.length.
    * Remaining inner terms and bundle roots are encoded as ~(holes.length+1) onward.
    *
    * NOTICE BIG This function is currently used for rule patterns, whereas toBundle is used
    * for regular terms and conclusion schemes. As a result, the `alt` set of terms mapped
    * to negative integers include (non-hole) leaves such as "tt" and "nil"; see (*) below.
    * Be aware of this small difference when calling them.
    */
  /**
    * Encodes a term with "holes": these are assigned Placeholders 1 through holes.length.
    * Subterms are considered implicit holes, so they are assigned distinct placeholders.
    * The roots of the terms in the bundle are special: they are all encoded as Placeholder(0).
    */
  def toBundles(implicit enc: Encoding): (Bundle, Bundle) = {

    val allVars = (src.vars ++ (precond flatMap (_.vars)) ++ target.vars).distinct

    //val srcTerms = src.template.split(RewriteRule.||>) map (_.split(RewriteRule.|||))
    val srcTerms = src.template.split(RewriteRule.|||) map (new Scheme.Template(allVars, _))
    val targetTerms = target.template.split(RewriteRule.|||) map (new Scheme.Template(allVars, _))

    // All holes including the addition of hyperterms for the subterms in the bundle
    /*
    val allSrcHoles = srcTerms.head.head :: srcHoles.toList ++
      (srcTerms.flatten flatMap (term => term.nodes filterNot (n => (n eq term) || (srcHoles contains n)/* (*) */
        /*n.isLeaf*/))) distinct
    val srcTermToPlaceholder: Map[Term, Placeholder] =
      allSrcHoles.zipWithIndex.toMap.mapValues(Placeholder) ++ (srcTerms.head.tail map ((_, Placeholder(0)))) ++
        (srcTerms.tail.zipWithIndex flatMap { case (terms, i) => terms map ((_, Placeholder(allSrcHoles.length + i))) })
    val srcPats = srcTerms.flatten flatMap (enc.toPatterns(_, srcTermToPlaceholder, srcHoles.length))

    val targetHoles = target.vars map (T(_))
    val targetTerms = target.template.split(RewriteRule.|||)

    assert(targetHoles.forall(_.isLeaf))
    val allTargetHoles = {
      // All terms other then the holes and the root need to become holes (they need a new hyper term)
      def internalSubterms(root: Term): Stream[Term] = root.nodes filter (subterm => !((subterm eq root) || subterm.isLeaf))
      targetTerms.head :: targetHoles.toList ++ (targetTerms flatMap internalSubterms)
    }

    val targetTermToPlaceholder = allTargetHoles.zipWithIndex.filterNot(kv => srcTermToPlaceholder.contains(kv._1)).
      toMap.mapValues(Placeholder) ++ (targetTerms.tail map ((_, Placeholder(0)))) ++ srcTermToPlaceholder
    val targetBundle = new Bundle(targetTerms flatMap (enc.toPatterns(_, targetTermToPlaceholder, targetHoles.length)) toList) // |-- dbg)
*/

    val rootPh = Placeholder(0)
    val varPhs = allVars.indices map (i => Placeholder(i + 1))

    def op(p1: Seq[Pattern], p2: Seq[Pattern]): Seq[Pattern] = Pattern.combinePatterns(p1, p2, (rootPh +: varPhs).toSet)

    val srcPats = (srcTerms map enc.toPatterns) reduceLeft op

    val targetPats = Pattern.shiftPatterns(srcPats, (targetTerms map enc.toPatterns) reduceLeft op, (rootPh +: varPhs).toSet)

    val srcBundle = new Bundle(
      if (precond.isEmpty) srcPats.toList
      else {
        assert(precond.length == 2 && precond.head.template.isLeaf) // TODO for now only this restricted case is allowed

        val precondPat = (for (pc <- precond) yield enc.toPatterns(pc)).flatten

        val combinedPats = Pattern.combinePatterns(srcPats, precondPat, varPhs.toSet)
        combinedPats.toList
      }
    )

    val targetBundle = new Bundle(targetPats.toList)

    (srcBundle, targetBundle)
  }


  def process(w: BaseRewriteEdge[Int], trie: Vocabulary[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding): List[BaseRewriteEdge[Int]] = {
    if (compiled.isEmpty) compile
    compiled.get.process(w, trie)
  }

  /**
    * Compiles the rule to patterns.
    * It is assumed that the same encoding will be used throughout the rule's lifetime.
    */
  def compile(implicit enc: Encoding) { compiled = Some(new CompiledRule(this)) }


  /** for debugging */
  def dumpCompiled(w: Writer)(implicit enc: Encoding) {
    if (precond.isEmpty)
      w.write(s"${src.template.toPretty} --> ${target.template.toPretty}\n")
    else
      w.write(s"${precond map (_.template.toPretty) mkString ", "} ==> ${src.template.toPretty} --> ${target.template.toPretty}\n")
    if (compiled.isEmpty) compile
    compiled.get.dump(w)
  }
}

object RewriteRule {
  /** Dont know yet
    */
  private class CompiledRule (origin: RewriteRule)(implicit enc: Encoding) extends LazyLogging {
    private val (pattern, conclusion): (Bundle, Bundle) = origin.toBundles
    val shards: Seq[Bundle] = pattern.shuffles
    val nHoles: Int = pattern.minValuationSize

    //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
    // For debuging this might be better: T((enc <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)
    // (but perhaps it is better to just write tests)
    private def fresh(wv: IndexedSeq[Int]): Int = enc.reserveIndex()

    private def sparseTargetLookup(sparsePattern: Seq[(Int, Int)], t: Vocabulary[Int, BaseRewriteEdge[Int]]): Option[Int] =
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

    def process(w: BaseRewriteEdge[Int], trie: Vocabulary[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
      val matcher = new Match(trie)(enc)
      val res = for (s <- shards;
                     valuation <- matcher.matchLookupUnify_*(s.patterns, w, new ImplValuation(nHoles))) yield {
        //println(s"valuation = ${valuation mkString " "}")
        val add = conclude(valuation, trie)
        logger.trace(s"added new words using ${s.patterns.map(_.mkString(" ")) mkString ", "}. words: ${add map (_ mkString " ") mkString ", "}")
        add
      }
      res.flatten
    }

    /** Return all rewrites after filling in all the holes.
      *
      * @param valuation The valution to fill.
      * @param trie containing all available rewrites
      * @return
      */
    private def conclude(valuation: Valuation, trie: Vocabulary[Int, BaseRewriteEdge[Int]]): List[BaseRewriteEdge[Int]] = {
      assert(valuation.length >= nHoles)

      val uid = new Uid
      val existentailEdges: ListBuffer[BaseRewriteEdge[Int]] = mutable.ListBuffer.empty

      def valuation_(i: Int): Option[HyperTerm] = {
        if (i < valuation.length && valuation.isDefined(i)) valuation(i)
        else if (i < valuation.length) None
        // --- introduces an existential
        else {
          val ident = new Identifier("ex?" + i, "variable", uid)
          val term = T(ident)
          existentailEdges.append(RewriteEdge(origin, Seq(enc --> ident, enc --> term)))
          Some(HyperTerm(enc --> term))
        }
      }

      // 0 1 4 -> -1 -2 -5 / 0 1 4 map Placeholder  | valuation(i)/0/existential
      val newSubterms: mutable.Map[Placeholder, Option[HyperTerm]] = mutable.Map.empty[Placeholder, Option[HyperTerm]] ++
        ((0 :: parameterIndexes) map (i => {(Placeholder(i), valuation_(i))}))

      // construct new hyperedges by replacing holes (undefined) with valuation elements and fresh hyperterms
      val f = for (pattern <- conclusion.patterns.reverse) yield {
        val updatedPattern: Pattern = new ImplPattern(pattern map {
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
      f ++ existentailEdges
    }

    /** for debugging */
    def dump(w: Writer)(implicit enc: Encoding): Unit = {
      def fmtHead(el: BaseHyperTerm) = el match {
        case HyperTerm(value) => (enc <-- value) getOrElse value
        case Placeholder(index) => s"~$index"
      }
      def fmtRest(el: BaseHyperTerm) = el match {
        case HyperTerm(value) => value
        case Placeholder(index) => s"~$index"
      }

      val premiseText =
        for (p <- pattern.patterns)
          yield s"${fmtHead(p.head)} ${p.tail map fmtRest mkString " "}"
      val conclusionText =
        for (p <- conclusion.patterns)
          yield s"${fmtHead(p.head)} ${p.tail map fmtRest mkString " "}"


      for (ln <- Utils.formatColumns(Seq(premiseText, conclusionText), colWidth = 25))
        w.write(s"    ${Utils.rtrim(ln)}\n")
    }
  }

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }
  type Category = Category.Value

  // TODO: make sure all literals are in encoding (for example elems, head, take)

  val `=>`: Identifier = I("=>", "operator") // directional rewrite
  val ||| : Identifier = I("|||", "operator") // parallel patterns or conclusions
  val ||> : Identifier = I("||>", "operator")

  val truth = new Scheme.Template(List(), relentless.BasicSignature.tt)

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
      case T(`=>`, List(lhs, rhs)) =>
        val left_v = varsUsed(lhs) map (_.leaf)
        val right_v = varsUsed(rhs) map (_.leaf)
        List(new RewriteRule(new Scheme.Template(left_v, lhs), new Scheme.Template(right_v, rhs), ruleType))
      case T(`=`, List(lhs, rhs)) =>
        val left_v = varsUsed(lhs) map (_.leaf)
        val right_v = varsUsed(rhs) map (_.leaf)
        val (l, r) = (new Scheme.Template(left_v, lhs), new Scheme.Template(right_v, rhs))
        List(new RewriteRule(l, r, ruleType), new RewriteRule(r, l, ruleType))
      case T(`||>`, List(precond, body)) =>
        val precond_v = varsUsed(precond) map (_.leaf)
        val precond_t = Seq(truth, new Scheme.Template(precond_v, precond))
        val rules = apply(body, vars, ruleType)
        rules map (r => new RewriteRule(r.src, r.target, r.precond ++ precond_t, r.ruleType))
      case other =>
        throw new RuntimeException(s"invalid syntax for rule: ${other toPretty}")
    }
  }
}