package synthesis.actions.operators

import java.io.{BufferedReader, PrintStream}

import syntax.AstSugar.{I, Term}
import synthesis.Language.Parser
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.ExplicitTerm
import synthesis.rewrites.{RewriteRule, Template}

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

    term.root.literal match {
      // operator = in the main is Let (adding a new hyperterm)
      case "->" => {
      // operator ->:
      //   For left is:
      //   1) a pattern - Locate (locating a pattern)
      //   2) a symbol - Locate (locating a symbol)
        val anchor = HyperTermIdentifier(I(term.root.literal, Programs.Kinds.NonConstructable.toString))
        val hyperPattern = createHyperPatternFromTerm(term.subtrees.head)
        val newState = new LocateAction(anchor, hyperPattern).apply(state)

      //   The right is:
      //   1) a symbol => easy reference
      //   2) a pattern => Elaborate (finding the pattern - is it needed in case we keep programs?!)
      //   3) a term => extract the left to to match the term (Generalize or extract methods)

      }
      // operator →: push stack
      // operator ←: pop stack
      // operator □: save state ?!
    }

    val output: String = term.toString()

    out.println(f"Out [${lines.length}]: $output")
    out.flush()
    lines += ((line, term))
    state
  }


  /* --- Privates --- */

  private val lines: mutable.Buffer[(String, Term)] = new mutable.ListBuffer[(String, Term)]()

  private def createTemplateFromTerm(term: Term, counter: ()=>Int=Stream.from(0).iterator.next): (HyperTermId, Set[Template]) = {
    val results = term.subtrees.map(createTemplateFromTerm(_, counter))
    val subtemplates = results.flatMap(_._2).toSet
    val subHyperTermId = results.map(_._1).map(ExplicitTerm[HyperTermId])
    val hyperTermId = HyperTermId(counter())
    (hyperTermId, subtemplates + Template(ExplicitTerm(hyperTermId), ExplicitTerm(HyperTermIdentifier(term.root)), subHyperTermId))
  }

  private def createHyperPatternFromTerm(term: Term): HyperPattern = {
    RewriteRule.createHyperPatternFromTemplates(createTemplateFromTerm(term)._2)
  }
}
