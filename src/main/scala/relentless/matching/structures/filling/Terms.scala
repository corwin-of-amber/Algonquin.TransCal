package relentless.matching.structures.filling

trait BaseHyperTerm extends Any { }

object BaseHyperTerm {
  def toInt(x: BaseHyperTerm): Int = x match {
    case ph: Placeholder => ~ph.value
    case ht: HyperTerm => ht.value
    case _ => throw new RuntimeException("No case for this type of BaseHyperTerm")
  }
}

case class HyperTerm(value: Int) extends AnyVal with BaseHyperTerm {}

case class Placeholder(value: Int) extends AnyVal with BaseHyperTerm {}
