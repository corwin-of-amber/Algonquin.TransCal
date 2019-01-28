package synthesis.actions

import synthesis.Programs
import synthesis.rewrites.RewriteRule
import synthesis.search.State

/**
  * @author tomer
  * @since 11/18/18
  */
case class ActionSearchState(programs: Programs, rewriteRules: Set[RewriteRule]) extends State
