package synthesis.actions

import synthesis.Programs
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import synthesis.search.{Operator, State}

/**
  * @author tomer
  * @since 11/18/18
  */
case class ActionSearchState(programs: Programs, rewriteRules: Set[Operator[RewriteSearchState]]) extends State
