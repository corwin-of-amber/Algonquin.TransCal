package transcallang

import syntax.Tree

import scala.util.parsing.combinator._

/** A dynamic trait of parsers to add new prefix operators in parse time.
  * @author tomer
  * @since 1/10/19
  */
trait Prefixer[Return, This <: Prefixer[Return, This]] extends RegexParsers {
  /** The known prefixes at the moment */
  def prefixes: Map[Int, Set[String]]

  /** A way to rebuild the the class */
  def build(prefixes: Map[Int, Set[String]]): This

  /** Adds a new prefix to known prefixes.
    *
    * @param operator The operator to add.
    * @param level The level ot the operator.
    * @return A Prefixer which knows the the new operator.
    */
  def addPrefix(operator: String, level: Int): This = {
    build(prefixes.updated(level, prefixes.getOrElse(level, Set.empty) + operator))
  }

  /** Create a parser that can parse the prefixes.
    *
    * @param lowerParser The parser under the prefixes.
    * @return A parser to parse with the prefixes.
    */
  def prefixParser(lowerParser: Parser[Return]): Parser[Return] = {
    val levels = prefixes.keys.toSet.toList.sorted.reverse
    var lastParser: Parser[Return] = lowerParser
    for(level <- levels) {
      val prefixesOperators: Set[String] = prefixes.getOrElse(level, Set.empty)
      def recursiveBuilder(expr: Return, operators: List[String]): Return = {
        operators match {
          case Nil => expr
          case operator :: otherOperators =>
            buildReturn(operator, recursiveBuilder(expr, otherOperators))
        }
      }

      val operatorsInLevel: Parser[String] = prefixesOperators.map(Parser[String](_)).reduce(_ | _)

      // Create the parser now
      val newParser = rep(operatorsInLevel) ~ lastParser ^^ { x => recursiveBuilder(x._2, x._1)}
      lastParser = newParser
    }
    lastParser
  }

  def buildReturn(operator: String, after: Return): Return
}
object Prefixer {
  val MIDDLE = 5
}
trait TermPrefixer extends Prefixer[Tree[Identifier], TermPrefixer] {
  override def buildReturn(operator: String, after: Tree[Identifier]): Tree[Identifier] = new Tree[Identifier](Identifier(operator), List(after))
}