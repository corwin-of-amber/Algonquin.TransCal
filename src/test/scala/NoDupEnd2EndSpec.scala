import java.io.{File, InputStream}

import ch.qos.logback.classic.Logger
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.read.ListAppender
import org.scalatest.concurrent.{Signaler, ThreadSignaler}
import org.scalatest.time.{Millis, Span}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, Suite}
import org.slf4j.LoggerFactory
import ui.Interpreter
import scala.collection.JavaConverters._


class NoDupEnd2EndSpec extends FlatSpec with Matchers with BeforeAndAfterAll {
  import NoDupEnd2EndSpec._

  val timeLimit: Span = Span(2000000, Millis)
  implicit val signaler: Signaler = ThreadSignaler

  def getVars(msg: String) = {
    "^\\(([^(]*)\\)".r.findFirstMatchIn(msg.trim).get.group(0).split(" ")
  }

  def getInput(logs: Seq[ILoggingEvent], index: Int) = {
    logs(index - 2)
  }

  def getOutput(logs: Seq[ILoggingEvent], index: Int) = {
    // +1 for locate +1 because line after tactic is a ---------------
    logs drop index+2 takeWhile(event => !event.getFormattedMessage.contains("------------"))
  }

  "Output" should "be not empty" in {
    appender.list.size() should be > 0
  }

  "The interpreter" should "define nodup (let command)" in {
    val logs = appender.list.asScala
    val letIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim == "let").map(_._2)
    letIndexes should not be empty
  }

  it should "locate the main term (locate commnad)" in {
    val logs = appender.list.asScala
    val locateIndexes = logs.zipWithIndex.filter(_._1.getFormattedMessage.trim == "locate").map(_._2)
    locateIndexes should not be empty
    val locateInput = getInput(logs, locateIndexes.head)
    val locateOutput = getOutput(logs, locateIndexes.head)
    getVars(locateInput.getFormattedMessage).length should be (2)
    locateOutput.length should be (1)
  }

  override protected def beforeAll(): Unit = {
    Interpreter.main(Array(Interpreter.getClass.getResource("/examples/NoDup.tc").getPath))
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