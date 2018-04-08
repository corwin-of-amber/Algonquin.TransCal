package relentless.rewriting

import com.typesafe.scalalogging.slf4j.LazyLogging

import collection.mutable
import syntax.Tree
import syntax.Scheme
import syntax.AstSugar._
import syntax.Identifier
import relentless.matching._


class CompiledRule(val shards: List[Bundle], val conclusion: Bundle, 
                   val nHoles: Int, val parameterIndexes: List[Int])(implicit enc: Encoding) {
  
  def this(pattern: Bundle, conclusion: Bundle, parameterIndexes: List[Int])(implicit enc: Encoding) =
    this(pattern.shuffles, conclusion, pattern.minValuationSize, parameterIndexes)

  def this(pattern: Scheme.Template, conclusion: Scheme.Template)(implicit enc: Encoding) = {
    this(enc.toBundles(pattern.vars map (T(_)):_*)(pattern.template.split(Rewrite.||>) map (_.split(Rewrite.|||))).bare, 
         enc.toBundle(conclusion.vars map (T(_)):_*)(conclusion.template.split(Rewrite.|||):_*),
         conclusion.vars map (v => 1 + (pattern.vars indexOf v))) 
    import Rewrite.RuleOps
    of (pattern.template =:> conclusion.template)
  }

  //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
  private def fresh(wv: Array[Int]) =
    enc.ntor --> T((enc.ntor <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)

  private def lookup(sparsePattern: Seq[(Int, Int)], t: Trie[Int, HyperEdge[Int]]): Option[Int] =
    t.sparseLookup(sparsePattern).map(_.target)

  def conclude(valuation: Array[Int], trie: Trie[Int, HyperEdge[Int]]) = {
    assert(valuation.length >= nHoles)
    
    def valuation_(i: Int) = if (i < valuation.length) valuation(i) else enc.ntor --> $TI("?"+i)  // --- introduces an existential
    val newSubterms = mutable.Map.empty[Int, Int] ++ (0::parameterIndexes map (i => (~i, valuation_(i))))
        
    //if (dbg != null) println(s"[rewrite] conclude from ${dbg.toPretty}  with  ${newSubterms mapValues (enc.ntor <--)}");
    
    // construct new tuples by replacing holes (negative integers) with valuation elements and fresh terms
    for (w <- conclusion.tuples.reverse) yield {
      val wv = w map { case x if x < 0 => newSubterms.getOrElse(x, x) case x => x }
      if (wv(1) < 0) {
        val wv1 = lookup((0, wv(0)) +: (for (i <- 2 until wv.length) yield (i, wv(i))), trie) getOrElse fresh(wv)
        newSubterms += wv(1) -> wv1
        wv(1) = wv1
      }
      HyperEdge(wv)
    }
  }
  
  /* For debugging and error report */
  var dbg: Term = null;
  def of(t: Term) = { dbg = t; this }

}


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
    (trie.words map (_(1)) toSet).size > 1000
  }

  def processRule(rule: CompiledRule, w: HyperEdge[Int]) {
    val valuation: Array[Option[HyperTerm]] = Stream.continually(None) take rule.nHoles toArray;
    for (s <- rule.shards) {
      for (valuation <- match_.matchLookupUnify_*(s.tuples map (_.toIndexedSeq), w, valuation)) {
        //println(s"valuation = ${valuation mkString " "}")
        val add = rule.conclude(valuation, trie)
        trace(rule, valuation, add)
        logger.trace(s"added new words using ${s.tuples.map(_.mkString(" ")) mkString(", ")}. words: ${add map (_ mkString " ") mkString ", "}")
        wq.enqueue (add:_*)
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
    Rewrite.nonMatches(trie.words, headSymbols:_*)

}


object Rewrite extends LazyLogging {
  val `=>` = I("=>", "operator")  // directional rewrite
  val ||| = I("|||", "operator")  // parallel patterns or conclusions
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
      case eqn @ T(`=>`, List(lhs, rhs)) => 
        val v = varsUsed(eqn) map (_.leaf)
        Seq(new CompiledRule(new Scheme.Template(v, lhs), new Scheme.Template(v, rhs)))
      case eqn @ T(`=`, List(lhs, rhs)) => 
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
    words filterNot (heads contains _(0))
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
    val rules = List(new CompiledRule(new Scheme.Template(x,y)(f:@(y,x)), new Scheme.Template(x,y)(y:@x)),
        new CompiledRule(new Scheme.Template(x,y)(y:@x), new Scheme.Template(x,y)(tt)),
        new CompiledRule(new Scheme.Template()(~tt), new Scheme.Template(x,y)(ff)))
    for (r <- rules) {
      for (v <- r.shards) logger.info(s"""{v.tuples map (_.mkString(" "))}""")
      logger.info("")
      logger.info(s"""{r.conclusion.tuples map (_.mkString(" "))}""")
      logger.info("-" * 60)
    }
    
    import java.io.FileWriter
    val encf = new FileWriter("enc")
    val pairs = enc.ntor.mapped.toList map { case (x,y) => (y,x) } sortBy (_._1);
    for ((k, v) <- pairs) { encf.write(s"${k} ${v}  (${v.getClass().getName()})\n"); }
    encf.close()
    
    
  }
}

