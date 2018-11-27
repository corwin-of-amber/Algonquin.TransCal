package synthesis

import structures.HyperGraphManyWithOrderToOne
import syntax.Tree

/**
  * @author tomer
  * @since 11/19/18
  */
class Programs(var hyperGraph: HyperGraphManyWithOrderToOne[HyperTerm, HyperTerm]) {
  def reconstruct(hyperTerm: HyperTerm): Iterator[Tree[Int]] = {
    null
  }
}
