package relentless.rewriting

import syntax.Scheme
import syntax.AstSugar._
import syntax.Identifier
import relentless.matching.Bundle
import relentless.matching.Encoding
import relentless.matching.Trie



class CompiledRule(val shards: List[Bundle], val conclusion: Bundle, 
                   val nHoles: Int, val parameterIndexes: List[Int])(implicit enc: Encoding) {
  
  def this(pattern: Bundle, conclusion: Bundle, parameterIndexes: List[Int])(implicit enc: Encoding) =
    this(pattern.shuffles, conclusion, pattern.minValuationSize, parameterIndexes)

    /*
  def this(pattern: Bundle, conclusion: Bundle)(implicit enc: Encoding) =
    this(pattern, conclusion, conclusion match {
      //case s: Scheme.Arity =>1 to s.arity toList
      case _ => 1 until pattern.minValuationSize toList  // not the best solution, assumes scheme ignores extraneous args
    })
    */
    
  def this(pattern: Scheme.Template, conclusion: Scheme.Template)(implicit enc: Encoding) =
    this(enc.toBundle(pattern.vars map (T(_)):_*)(pattern.template.split(Rewrite.|||):_*).bare, 
         enc.toBundle(conclusion.vars map (T(_)):_*)(conclusion.template),
         conclusion.vars map (v => 1 + (pattern.vars indexOf v)))
  
  def conclude(valuation: Array[Int], trie: Trie[Int]) = {
    assert(valuation.length >= nHoles)
    
    val newSubterms = collection.mutable.Map.empty[Int, Int] ++ (0::parameterIndexes map (i => (~i, valuation(i))))
        
    //def fresh(wv: Array[Int]) = enc.ntor --> new Uid  // -- more efficient? but definitely harder to debug
    def fresh(wv: Array[Int]) = enc.ntor --> T((enc.ntor <-- wv(0)).asInstanceOf[Identifier], wv.drop(2) map enc.asTerm toList)

    def lookup(ivs: Seq[(Int, Int)], t: Trie[Int]): Option[Int] = ivs match {
      case Nil => Some(t.words.head(1))
      case (i, v) +: ivs => t.get(i, v) match {
        case None => None
        case Some(t) => lookup(ivs, t)
      }
    }
    
    //println("--- " + newSubterms)
    
    for (w <- conclusion.tuples.reverse) yield {
      val wv = w map { case x if x < 0 => newSubterms.getOrElse(x, x) case x => x }
      if (wv(1) < 0) {
        val wv1 = lookup((0, wv(0)) +: (for (i <- 2 until wv.length) yield (i, wv(i))), trie) getOrElse fresh(wv)
        newSubterms += wv(1) -> wv1
        wv(1) = wv1
      }
      //println(s"[ ${w mkString " "} -->")
      //println(s"  ${wv mkString " "} ]")
      wv
    }
  }
  
}



object Rewrite {
  
  val `=>` = I("=>", "operator")  // directional rewrite
  val ||| = I("|||", "operator")
  
  implicit class RuleOps(private val t: Term) extends AnyVal {
    def =:>(s: Term) = T(`=>`)(t, s)
    def |||(s: Term) = T(Rewrite.|||)(t, s)
  }

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
}

