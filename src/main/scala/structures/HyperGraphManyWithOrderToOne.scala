package structures

import synthesis.HyperTerm
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]]
