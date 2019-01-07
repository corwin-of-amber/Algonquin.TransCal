package synthesis.actions.operators

import java.io.{BufferedReader, PrintStream}

import syntax.AstSugar.Term
import synthesis.Language.Parser
import synthesis.actions.ActionSearchState

import scala.collection.mutable

/** Getting actions from user.
  * @author tomer
  * @since 11/18/18
  */
class UserAction(in: BufferedReader, out:PrintStream, parser: Parser[Term]) extends Action {

  /* --- Public --- */

  override def apply(state: ActionSearchState): ActionSearchState = {
    out.print(f"In [${lines.length}]: ")
    out.flush()
    val line: String = in.readLine()
    val term = parser.apply(line)

    // TODO: Add parsing functionality
    // operator = in the main is Let (adding a new hyperterm)
    // operator ->:
    //   For left is:
    //   1) a pattern - Locate (locating a pattern)
    //   2) a symbol - Locate (locating a symbol)
    //   The right is:
    //   1) a symbol => easy reference
    //   2) a pattern => Elaborate (finding the pattern - is it needed in case we keep programs?!)
    //   3) a term => extract the left to to match the term (Generalize or extract methods)
    // operator →: push stack
    // operator ←: pop stack
    // operator □: save state ?!
    val output: String = term.toString()

    out.println(f"Out [${lines.length}]: $output")
    out.flush()
    lines += ((line, term))
    state
  }


  /* --- Privates --- */

  private val lines: mutable.Buffer[(String, Term)] = new mutable.ListBuffer[(String, Term)]()
}
