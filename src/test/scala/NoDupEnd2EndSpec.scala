import java.io.{File, InputStream}

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import org.scalatest.concurrent.{Signaler, ThreadSignaler}
import org.scalatest.time.{Millis, Span}
import org.scalatest._
import org.slf4j.LoggerFactory
import ui.Interpreter
import ch.qos.logback.classic.Level
import org.scalatest

import scala.collection.JavaConverters._
import scala.util.matching.Regex


class NoDupEnd2EndSpec extends FlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfter {
  import NoDupEnd2EndSpec._

  val timeLimit: Span = Span(2000000, Millis)
  implicit val signaler: Signaler = ThreadSignaler

  var logs: List[ILoggingEvent] = appender.list.asScala.toList

  before {
    logs = appender.list.asScala.toList.filter(p => p.getLevel == Level.INFO)
  }

  override protected def beforeAll(): Unit = {
    Interpreter.main(Array(Interpreter.getClass.getResource("/examples/NoDup.tc").getPath))
  }

  def getVars(msg: String) = {
    "^\\(([^(]*)\\)".r.findFirstMatchIn(msg.trim).get.group(1).split(" ")
  }

  def getInput(logs: Seq[ILoggingEvent], index: Int) = {
    if (logs(index - 1).getFormattedMessage.trim.startsWith("DeductionHints")) logs(index - 2)
    else logs(index - 1)
  }

  def getOutput(logs: Seq[ILoggingEvent], index: Int) = {
    // +1 for locate +1 because line after tactic is a ---------------
    logs drop index+2 takeWhile(event => !event.getFormattedMessage.contains("------------"))
  }

  "Output" should "be not empty" in {
    appender.list.size() should be > 0
  }

  // When transcal receives a "=" it will use the let tactic
  "The interpreter" should "define nodup (let command)" in {
    val letIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim == "let").map(_._2)
    letIndexes should not be empty
  }

  // When transcal receives a "->" with number to the right it will locate the term to left
  it should "locate the main term (locate commnad)" in {
    val locateIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim == "locate").map(_._2)
    locateIndexes should not be empty
    val locateInput = getInput(logs, locateIndexes.head)
    val locateOutput = getOutput(logs, locateIndexes.head)
    getVars(locateInput.getFormattedMessage).length should be (2)
    locateOutput.length should be (1)
    locateOutput.head.getFormattedMessage.trim shouldEqual "¬(elem x xs) ∧ nodup xs"
  }

  // When transcal receives a "->" with ? to the right define the new function
  it should "generalize the located term into a new function (generalize commnad)" in {
    val genIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim.startsWith("generalize")).map(_._2)
    genIndexes should not be empty
    // We will have only the new function name as a var bgecause x/xs are defined
    val genInput = getInput(logs, genIndexes.head)
    val genOutput = getOutput(logs, genIndexes.head)
    getVars(genInput.getFormattedMessage).length should be (1)
    getVars(genInput.getFormattedMessage).head shouldEqual "nodup'"
    genOutput.length should be (2)
    // get message with param replacement and remove as at print start
    val generalizedExpression = genOutput.last.getFormattedMessage.trim.substring(2)
    // this means nodup' is the lambda (α β ↦ (α ‖ elems β) ∧ nodup β)
    // If this fails it is because there might be more ways to express this lambda.
    // In case of failure we should generalize the assertion
    generalizedExpression.trim shouldEqual "(α β ↦ (α ‖ elems β) ∧ nodup β) {x} xs"
  }

  // When transcal receives a "->" with a number to the left and a pattern to the right
  // it will elaborate the expression
  it should "transform term into the requested pattern (elaborate commnad)" in {
    val elabIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim.startsWith("elaborate")).map(_._2)
    elabIndexes should not be empty
    // We will have only the new function name as a var bgecause x/xs are defined
    val elabInput = getInput(logs, elabIndexes.head)
    val elabOutput = getOutput(logs, elabIndexes.head)
    // The pattern has 4 holes so the requested term has 4 vars
    getVars(elabInput.getFormattedMessage).length should be (4)
    elabOutput.length should be > 0
    verifyElaborateIO(elabOutput, elabVerifier = right => {
      val elaborated = raw"(.*)(?:/\\|∧)(.*)(?:/\\|∧)(.*)(?:/\\|∧)(.*)".r.findFirstMatchIn(right).get
      for (i <- 1 to 4) {
        val text = elaborated.group(i)
        defaultVerifier(text)
      }})
    // TODO: Logic testing that the found program is equivalent to nodup
  }

  it should "locate pattern and transform term into the first requested pattern (locate and elaborate commnad)" in {
    val elabIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim.startsWith("elaborate")).map(_._2)
    elabIndexes should not be empty
    // We will have only the new function name as a var bgecause x/xs are defined
    val elabInput = getInput(logs, elabIndexes.drop(1).head)
    val elabOutput = getOutput(logs, elabIndexes.drop(1).head)
    // The pattern has 4 holes so the requested term has 4 vars
    getVars(elabInput.getFormattedMessage).length should be (4)
    elabOutput.length should be > 0
    verifyElaborateIO(elabOutput,
      locateVerifier = left => {
    val located = raw"x\s*∉\s*(.*)\s*(?:/\\|∧)\s*x'\s*∉\s*(.*)".r.findFirstMatchIn(left)
    located should not be None
    },
    elabVerifier = right => {
      val elaborated = raw"(.*)\s*‖\s*(.*)".r.findFirstMatchIn(right)
      elaborated should not be None
    })
  }

  // TODO: finish rest of app;ied tactics

  private def defaultVerifier(text: String): Unit = {
    text.trim should not equal "?"
    text.trim should not equal "_"
    text.trim should not equal ""
  }

  private def verifyElaborateIO(elabOutput: Seq[ILoggingEvent],
                                locateVerifier: String => Unit = defaultVerifier,
                                elabVerifier: String => Unit = defaultVerifier): Unit = {
    val matches = "(.*)-->(.*)".r.findFirstMatchIn(elabOutput.last.getFormattedMessage).get
    val left = matches.group(1)
    val right = matches.group(2)
    locateVerifier(left)
    elabVerifier(right)
  }
}

object NoDupEnd2EndSpec {
  var output: String = ""
  val appender = {
    val root = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    val it = root.iteratorForAppenders()
    while (it.hasNext) {
      println(it.next.getName)
    }
    root.getAppender("list").asInstanceOf[ListAppender[ILoggingEvent]]
  }
}