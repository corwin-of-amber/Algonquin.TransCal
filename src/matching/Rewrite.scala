package matching

import syntax.Scheme
import syntax.AstSugar.T



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