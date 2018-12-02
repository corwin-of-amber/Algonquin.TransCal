package synthesis.actions.operators

import java.io.{BufferedReader, PrintStream}

import synthesis.actions.ActionSearchState

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/18/18
  */
class UserAction(in: BufferedReader, out:PrintStream) extends Action {

  private val lines: mutable.Buffer[(String, String)] = new mutable.ListBuffer[(String, String)]()

  override def apply(state: ActionSearchState): ActionSearchState = {
    out.print(f"In [${lines.length}]: ")
    out.flush()
    val line = in.readLine()
    out.println(f"Out [${lines.length}]: $line")
    out.flush()
    val a: (String, String) = (line, line)
    lines += a
    state
  }
}
