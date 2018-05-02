package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging
import relentless.matching._
import syntax.AstSugar._
import syntax.{Identifier, Scheme, Tree}

import scala.collection.immutable


/** Representing the rewrite system
  *
  * The rewriting is done by building a Trie of terms.
  * The full term can be recreated using reconstruct
  */
class Rewriter(init: Seq[BaseRewriteEdge[Int]], compiledRules: List[CompiledRule], val trie: Trie[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) extends LazyLogging {

  import Rewriter._

  import collection.mutable

  def this(init: Seq[BaseRewriteEdge[Int]], compiledRules: List[CompiledRule], directory: Tree[Trie.DirectoryEntry])(implicit enc: Encoding) =
    this(init, compiledRules, new Trie[Int, BaseRewriteEdge[Int]](directory))

  private val match_ = new Match(trie)(enc)

  private val wq = mutable.Queue.empty[BaseRewriteEdge[Int]] ++ init
  private val ws = mutable.Set.empty[BaseRewriteEdge[Int]]

  def apply(): Unit = {
    while (wq.nonEmpty && !trie.exceeded) {
      val w = wq.dequeue()
      if (ws add w) {
        work(w)
      }
    }
  }

  def stream(): Stream[Seq[BaseRewriteEdge[Int]]] = {
    var i = 0
    Reconstruct.whileYield(wq.nonEmpty) {
      val w = wq.dequeue()
      if (ws add w) {
        work(w)
      }
      val gm = matches(RuleBasedTactic.Markers.goal.leaf) // perhaps generalize to other markers?
      val gmAdded = gm drop i
      i = gm.length
      gmAdded
    }
  }

  def work(w: BaseRewriteEdge[Int]): Unit = {
    //println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")

    if (trie.words.contains(w))
      logger.debug(s"Readding a word to trie. word: $w")
    trie add w
    logger.trace(s"working on word ${w mkString " "}")
    for (r <- compiledRules) {
      processRule(r, w)
    }

    //for (g <- goal) processRule(g, w)
  }

  private def processRule(rule: CompiledRule, w: BaseRewriteEdge[Int]): Unit = {
    for (s <- rule.shards) {
      val patterns: List[Pattern] = s.patterns
      for (valuation <- match_.matchLookupUnify_*(patterns, w, new Valuation(rule.nHoles))) {
        //println(s"valuation = ${valuation mkString " "}")
        val add = rule.conclude(valuation, trie)
        trace(rule, for(i <- (0 until valuation.length).toArray) yield {if (valuation.isDefined(i)) valuation(i).value else 0}, add)
        logger.trace(s"added new words using ${s.patterns.map(_.mkString(" ")) mkString (", ")}. words: ${add map (_ mkString " ") mkString ", "}")
        wq.enqueue(add: _*)
      }
    }
  }

  def matches(headSymbol: Identifier): Seq[BaseRewriteEdge[Int]] = {
    trie.realGet(0, enc.ntor --> headSymbol)
  }

  def nonMatches(headSymbols: Identifier*): Seq[BaseRewriteEdge[Int]] =
    Rewriter.nonMatches(trie.words, headSymbols: _*)

}


object Rewriter extends LazyLogging {
  val `=>` = I("=>", "operator") // directional rewrite
  val ||| = I("|||", "operator") // parallel patterns or conclusions
  val ||> = I("||>", "operator")

  implicit class RuleOps(private val t: Term) extends AnyVal {
    def =:>(s: Term): Tree[Identifier] = T(`=>`)(t, s)

    def |||(s: Term): Tree[Identifier] = T(Rewriter.|||)(t, s)

    def ||>(s: Term): Tree[Identifier] = T(Rewriter.||>)(t, s)
  }

  import syntax.Formula
  import syntax.Formula._

  Formula.INFIX ++= List(`=>` -> O("=>", 5), `|||` -> O("|||", 5))

  def compileRules(vars: List[Term], rulesSrc: List[Term])(implicit enc: Encoding): List[CompiledRule] = {

    def varsUsed(t: Term) = vars filter t.leaves.contains

    //println(rulesSrc map (_ toPretty))

    rulesSrc flatMap {
      case eqn@T(`=>`, List(lhs, rhs)) =>
        val v = varsUsed(eqn) map (_.leaf)
        Seq(new CompiledRule(new Scheme.Template(v, lhs), new Scheme.Template(v, rhs)))
      case eqn@T(`=`, List(lhs, rhs)) =>
        val v = varsUsed(eqn) map (_.leaf)
        val (l, r) = (new Scheme.Template(v, lhs), new Scheme.Template(v, rhs))
        Seq(new CompiledRule(l, r), new CompiledRule(r, l))
      case other =>
        throw new RuntimeException(s"invalid syntax for rule: ${other toPretty}")
    }
  }

  def compileRule(ruleSrc: Scheme.Template)(implicit enc: Encoding): List[CompiledRule] =
    compileRules(ruleSrc.vars map (T(_)), List(ruleSrc.template))

  def nonMatches[HE <: BaseHyperEdge[Int]](words: Seq[HE], headSymbols: Identifier*)(implicit enc: Encoding): Seq[HE] = {
    val heads = headSymbols map (enc.ntor -->)
    words filterNot (heads contains _ (0))
  }

  /**
    * Used for misc debugging
    */
  object trace {
    def apply[HE <: BaseHyperEdge[Int]](rule: CompiledRule, valuation: Array[Int], conclusion: Iterable[HE]) {
      for (w <- conclusion)
        productions += w.toList -> (rule, valuation)
    }

    val productions: collection.mutable.Map[List[Int], (CompiledRule, Array[Int])] = collection.mutable.Map.empty
  }


  def main(args: Array[String]): Unit = {
    implicit val enc = new Encoding
    import examples.BasicSignature._
    //val r = new CompiledRule(new Scheme.Template(x)(`⇒:`(tt, x)), new Scheme.Template(x)(id(x)))
    val rules = List(new CompiledRule(new Scheme.Template(x, y)(f :@ (y, x)), new Scheme.Template(x, y)(y :@ x)),
      new CompiledRule(new Scheme.Template(x, y)(y :@ x), new Scheme.Template(x, y)(tt)),
      new CompiledRule(new Scheme.Template()(~tt), new Scheme.Template(x, y)(ff)))
    for (r <- rules) {
      for (v <- r.shards) logger.info(s"""{v.tuples map (_.mkString(" "))}""")
      logger.info("")
      logger.info(s"""{r.conclusion.tuples map (_.mkString(" "))}""")
      logger.info("-" * 60)
    }

    import java.io.FileWriter
    val encf = new FileWriter("enc")
    val pairs = enc.ntor.mapped.toList map { case (x, y) => (y, x) } sortBy (_._1);
    for ((k, v) <- pairs) {
      encf.write(s"${k} ${v}  (${v.getClass().getName()})\n");
    }
    encf.close()
  }
}

// Note: need to inherit base class to gain the ability to define a case class
abstract class BaseRewriteEdge[T](edgeType: T, target: T, params: Seq[T]) extends
  BaseHyperEdge[T](edgeType, target, params) {}

case class OriginalEdge[T](override val edgeType: T, override val target: T, override val params: Seq[T]) extends
  BaseRewriteEdge[T](edgeType, target, params) {}
case object OriginalEdge {
    def apply[T](seq: Seq[T]): OriginalEdge[T] = OriginalEdge[T](seq(0), seq(1), seq drop 2)
}

case class RewriteEdge[T](override val edgeType: T, override val target: T, override val params: Seq[T]) extends
  BaseRewriteEdge[T](edgeType, target, params) {}
case object RewriteEdge {
  def apply[T](seq: Seq[T]): RewriteEdge[T] = RewriteEdge[T](seq(0), seq(1), seq drop 2)
}