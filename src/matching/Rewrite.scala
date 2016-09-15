package matching

import syntax.Scheme
import syntax.AstSugar._



class CompiledRule(val shards: List[Bundle], val conclusion: Scheme, 
                   val nHoles: Int, val parameterIndexes: List[Int])(implicit enc: Encoding) {
  
  def this(pattern: Bundle, conclusion: Scheme, parameterIndexes: List[Int])(implicit enc: Encoding) =
    this(pattern.shuffles, conclusion, pattern.minValuationSize, parameterIndexes)

  def this(pattern: Bundle, conclusion: Scheme)(implicit enc: Encoding) =
    this(pattern, conclusion, conclusion match {
      case s: Scheme.Arity =>1 to s.arity toList
      case _ => 1 until pattern.minValuationSize toList  // not the best solution, assumes scheme ignores extraneous args
    })
    
  def this(pattern: Scheme.Template, conclusion: Scheme)(implicit enc: Encoding) =
    this(enc.toBundle(pattern.vars map (T(_)):_*)(pattern.template), conclusion)
  
  def conclude(valuation: Array[Int]) = {
    assert(valuation.length >= nHoles)
    val parameters = parameterIndexes map (i => try { enc.asTerm(valuation(i)) } catch 
        { case _: ClassCastException => throw new RuntimeException(s"invalid valuation ${valuation mkString " "}; element [${i}] = ${valuation(i)} cannot be mapped to a term") })

    import syntax.AstSugar._
    println(s"parameters = ${parameters map (_.toPretty) mkString " "}")
    
    conclusion(parameters)
  }
  
}



object Rewrite {
  
  val `=>` = I("=>", "operator")  // directional rewrite
  
  
  def compileRules(vars: List[Term], rulesSrc: List[Term])(implicit enc: Encoding) = {
    
    def varsUsed(t: Term) = vars filter t.leaves.contains
          
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

