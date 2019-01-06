package synthesis

import syntax.Identifier

/**
  * @author tomer
  * @since 11/16/18
  */
sealed trait HyperTerm
case class HyperTermId(id: Int) extends HyperTerm

case class HyperTermIdentifier(identifier: Identifier) extends HyperTerm
