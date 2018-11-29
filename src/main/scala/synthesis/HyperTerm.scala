package synthesis

/**
  * @author tomer
  * @since 11/16/18
  */
trait HyperTerm
object HyperTerm {
  def toInt(hyperTerm: HyperTerm): Int = hyperTerm match {
    case PlaceHolder(id) => id
    case Value(id) => id
  }
}
case class PlaceHolder(id: Int) extends HyperTerm
case class Value(id: Int) extends HyperTerm
