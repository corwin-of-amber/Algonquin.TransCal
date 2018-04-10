package relentless.matching

trait BaseHyperTerm extends Any {}

case class HyperTerm(value: Int) extends AnyVal with BaseHyperTerm {}
case class Placeholder(value: Int) extends AnyVal with BaseHyperTerm {}
