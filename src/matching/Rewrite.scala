package matching

import syntax.Scheme



class CompiledRule(val shards: List[Bundle], val conclusion: Scheme, 
                   val nHoles: Int, val parameterIndexes: List[Int])(implicit enc: Encoding) {
  
  def conclude(valuation: Array[Int]) = {
    assert(valuation.length >= nHoles)
    val parameters = parameterIndexes map (i => enc.asTerm(valuation(i)))

    import syntax.AstSugar._
    println(s"parameters = ${parameters map (_.toPretty) mkString " "}")
    
    conclusion(parameters)
  }
  
}