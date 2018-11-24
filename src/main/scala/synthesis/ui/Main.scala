package synthesis.ui

import scala.io.Source

/**
  * @author tomer
  * @since 11/24/18
  */
object Main {

  import org.rogach.scallop.ScallopConf

  class CommandLineConfiguration(arguments: Seq[String]) extends ScallopConf(arguments) {
    val file = opt[String]()
    verify()
  }

  def main(args: Array[String]): Unit = {
    val conf = new CommandLineConfiguration(args)
    val userSource = conf.file.map(Source.fromFile).toOption.getOrElse(Source.stdin)
    val interpreter = new Interpreter(userSource)
  }
}
