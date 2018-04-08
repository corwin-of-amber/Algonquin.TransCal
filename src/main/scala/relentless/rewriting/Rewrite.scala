package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging

import collection.mutable
import syntax.Tree
import syntax.Scheme
import syntax.AstSugar._
import syntax.Identifier
import relentless.matching._
import relentless.rewriting.Rewrite.trace

import scala.collection.immutable





/** Representing the rewrite system
  *
  * The rewriting is done by building a Trie of terms.
  * The full term can be recreated using reconstruct
  */
class Rewrite(init: Seq[HyperEdge[Int]], compiledRules: List[CompiledRule], val trie: Trie[Int, HyperEdge[Int]])(implicit enc: Encoding) extends LazyLogging {

  import collection.mutable
  import Rewrite._

  def this(init: Seq[HyperEdge[Int]], compiledRules: List[CompiledRule], directory: Tree[Trie.DirectoryEntry])(implicit enc: Encoding) =
    this(init, compiledRules, new Trie[Int, HyperEdge[Int]](directory))

  val match_ = new Match(trie)(enc)

  val wq = mutable.Queue.empty[HyperEdge[Int]] ++ init
  val ws = mutable.Set.empty[HyperEdge[Int]]

  def apply() {
    while (!wq.isEmpty && !exceeded) {
      val w = wq.dequeue()
      if (ws add w) {
        work(w)
      }
    }
  }

  def stream() = {
    var i = 0
    Reconstruct.whileYield(!wq.isEmpty) {
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

  def work(w: HyperEdge[Int]) {
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

  def exceeded = {
    //trie.subtries(1).size > 1000
    //println(s"size of PEG  ${(trie.words map (_(1)) toSet).size}")
    (trie.words map (_ (1)) toSet).size > 1000
  }

  def processRule(rule: CompiledRule, w: HyperEdge[Int]) {
    for (s <- rule.shards) {
      def convertList(l: Array[Int]): IndexedSeq[BaseHyperTerm] = Pattern.toHyperTermBase(l)
      val patterns: List[Pattern] = s.tuples map convertList map (new Pattern(_))
      for (valuation <- match_.matchLookupUnify_*(patterns, w, new Valuation(rule.nHoles))) {
        //println(s"valuation = ${valuation mkString " "}")
        val add = rule.conclude(valuation, trie)
        trace(rule, for(i <- (0 until valuation.length).toArray) yield {if (valuation.isDefined(i)) valuation(i).value else 0}, add)
        logger.trace(s"added new words using ${s.tuples.map(_.mkString(" ")) mkString (", ")}. words: ${add map (_ mkString " ") mkString ", "}")
        wq.enqueue(add: _*)
      }
    }
  }

  def matches(headSymbol: Identifier) = {
    trie.get(0, enc.ntor --> headSymbol) match {
      case Some(t) => t.words
      case _ => Seq.empty
    }
  }

  def nonMatches(headSymbols: Identifier*) =
    Rewrite.nonMatches(trie.words, headSymbols: _*)

}


object Rewrite extends LazyLogging {
  val `=>` = I("=>", "operator") // directional rewrite
  val ||| = I("|||", "operator") // parallel patterns or conclusions
  val ||> = I("||>", "operator")

  implicit class RuleOps(private val t: Term) extends AnyVal {
    def =:>(s: Term) = T(`=>`)(t, s)

    def |||(s: Term) = T(Rewrite.|||)(t, s)

    def ||>(s: Term) = T(Rewrite.||>)(t, s)
  }

  import syntax.Formula
  import syntax.Formula._

  Formula.INFIX ++= List(`=>` -> O("=>", 5), `|||` -> O("|||", 5));

  def compileRules(vars: List[Term], rulesSrc: List[Term])(implicit enc: Encoding) = {

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

  def compileRule(ruleSrc: Scheme.Template)(implicit enc: Encoding) =
    compileRules(ruleSrc.vars map (T(_)), List(ruleSrc.template))

  def nonMatches(words: Seq[HyperEdge[Int]], headSymbols: Identifier*)(implicit enc: Encoding) = {
    val heads = headSymbols map (enc.ntor -->)
    words filterNot (heads contains _ (0))
  }

  /**
    * Used for misc debugging
    */
  object trace {
    def apply(rule: CompiledRule, valuation: Array[Int], conclusion: Iterable[HyperEdge[Int]]) {
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

