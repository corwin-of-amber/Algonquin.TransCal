package synthesis.rewrites

import structures.HyperGraphManyWithOrderToOne
import synthesis.Term
import synthesis.search.State

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteSearchState(val graph: HyperGraphManyWithOrderToOne[Term, Term]) extends State
