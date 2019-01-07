package synthesis.ui

import synthesis.SimpleRewriteRulesDB

/**
  * @author tomer
  * @since 11/24/18
  */
object Main extends App {

  println(SimpleRewriteRulesDB.rewriteRules)
  println(SimpleRewriteRulesDB.rewriteRules.size)
//  import org.rogach.scallop.ScallopConf
//
//  class CommandLineConfiguration(arguments: Seq[String]) extends ScallopConf(arguments) {
//    val file = opt[String]()
//    verify()
//  }
//
//  val conf = new CommandLineConfiguration(args)
//  val userInput: BufferedReader = conf.file.map(Source.fromFile).map(_.bufferedReader()).getOrElse(Console.in)
//  val userOutput: PrintStream = conf.file.map(name => new PrintStream(name + ".out")).getOrElse(Console.out)
//  val interpreter = new Interpreter(userInput, userOutput)
//  interpreter.start
}
