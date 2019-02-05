package language

import com.typesafe.scalalogging.LazyLogging
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}

import scala.util.parsing.combinator._

/** A dynamic trait of parsers to add new operators in parse time.
  * @author tomer
  * @since 1/10/19
  */
trait Infixer[Return, This <: Infixer[Return, This]] extends RegexParsers with LazyLogging {
  /** The known left operators at the moment */
  def lefters: Map[Int, Set[String]]

  /** The known right operators at the moment */
  def righters: Map[Int, Set[String]]

  /** A way to rebuild the the class */
  def build(lefters: Map[Int, Set[String]], righters: Map[Int, Set[String]]): This

  /** Adds a new operator to known prefixes.
    *
    * @param operator The operator to add.
    * @param level The level of the operator.
    * @param righter Is the operators stick to the right.
    * @return A Infixer which knows the the new operator.
    */
  def addInfix(operator: String, level: Int, righter:Boolean): This = {
    if (righter)
      build(lefters, righters.updated(level, righters.getOrElse(level, Set.empty) + operator))
    else
      build(lefters.updated(level, lefters.getOrElse(level, Set.empty) + operator), righters)
  }

  /** Create a parser that can parse the operators with order.
    *
    * @param lowerParser The parser under the operators.
    * @return A parser to parse with the prefixes.
    */
  def operatorsParser(lowerParser: Parser[Return]): Parser[Return] = {
    val levels = (lefters.keys ++ righters.keys).toSet.toList.sorted.reverse
    var lastParser: Parser[Return] = lowerParser
    for(level <- levels) {
      val leftOperators: Set[String] = lefters.getOrElse(level, Set.empty)
      val rightOperators: Set[String] = righters.getOrElse(level, Set.empty)
      def recursiveBuilder(exprs: List[Return], operators: List[String]): Return = {
        operators match {
          case Nil => exprs.head
          case operator :: otherOperators =>
            if (leftOperators.contains(operator)) {
              val expr1 :: expr2 :: otherExpr = exprs
              recursiveBuilder(buildReturn(expr1, operator, expr2) :: otherExpr, otherOperators)
            } else {
              val expr1 :: otherExpr = exprs
              buildReturn(expr1, operator, recursiveBuilder(otherExpr, otherOperators))
            }
        }
      }

      val operatorsInLevel: Parser[String] = {
        val head::tail = (leftOperators | rightOperators).toList
        tail.foldLeft[Parser[String]](head)((paresr, operator) => paresr | operator)
      }

      // Create the parser now
      val newParser = lastParser ~ rep(operatorsInLevel ~ lastParser) ^^ { x =>
        if (x._2.nonEmpty) logger.trace(s"infix level $level - $x")
        recursiveBuilder(x._1 :: x._2.map(_._2), x._2.map(_._1))
      }
      lastParser = newParser
    }
    lastParser
  }

  def buildReturn(left: Return, operator: String, right: Return): Return
}

object Infixer {
  val MIDDLE = 5
  val HIGH = 50
}

trait TermInfixer extends Infixer[Term, TermInfixer] {
  override def buildReturn(left: Term, operator: String, right: Term): Term = new Tree[Identifier](new Identifier(operator), List(left, right))
}