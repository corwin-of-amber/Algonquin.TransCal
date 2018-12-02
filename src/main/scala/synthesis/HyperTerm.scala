package synthesis

import syntax.Identifier

/**
  * @author tomer
  * @since 11/16/18
  */
trait HyperTerm
case class HyperTermId(id: Int) extends HyperTerm
case class HyperTermIdentifier(val identifier: Identifier) extends HyperTerm
