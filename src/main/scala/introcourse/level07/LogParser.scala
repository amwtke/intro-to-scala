package introcourse.level07

import scala.util.{Failure, Success, Try}

/**
  * The exercises here are adapted from: http://www.cis.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
  *
  * Let's finish off this course by building a CLI program!
  * We will build a program to parse a log file (containing Info, Warn and Error messages) and print out errors over a severity level.
  *
  * Finish each exercise and we will head over to `Main.scala` to hook it all up with the CLI.
  */
object LogParser {

  /**
    * Here is how a log file may look.
    *
    * The `|` at the start of each line can be ignored. This is how Scala represents a multi-line String.
    */
  val logFile: String =
    """|I,147,mice in the air
       |W,149,could've been bad
       |E,5,158,some strange error
       |E,2,148,istereadea""".stripMargin

  /**
    * Let's define an ADT to represent all possible log messages.
    *
    * Hint: Add `sealed` in front of the `trait`s below to make sure you can only extend them from within this file.
    */

  /**
    * Start with all the possible log levels
    * - Info
    * - Warning
    * - Error with (severity: Int)
    */
  sealed trait LogLevel

  case object Info extends LogLevel

  case object Warning extends LogLevel

  case class Error(severity: Int) extends LogLevel

  /**
    * Now create an ADT for `LogMessage`, where `LogMessage` can be one of two possibilities:
    * - KnownLog with (logLevel, timestamp, message)
    * - UnknownLog with (message)
    */
  type Timestamp = Int

  sealed trait LogMessage

  case class KnownLog(logLevel: LogLevel, timestamp: Timestamp, message: String) extends LogMessage

  case class UnknownLog(message: String) extends LogMessage

  case object MalformedLog extends LogMessage

  /**
    * - Once you have defined your data types:
    * 1. Remove `import Types._` from [LogParserTest.scala](src/test/scala/introcourse/level07/LogParserTest.scala)
    * 2. Comment out the contents of src/test/scala/introcourse/level07/Types.scala
    */

  /**
    * We want to parse log messages. In order to do so, we need a safe way to parse integers in those messages.
    *
    * One possibility is to define a function that converts a `String` to an `Option[Int]`
    *
    * Hint: You may copy `parseIntSafe` and `tryToOption` from TryExercises
    */
  def parseIntOption(str: String): Option[Int] = Try(str.toInt) match {
    case Success(value) => Some(value)
    case Failure(_) => None
  }

  /**
    * Define a function to parse an individual log message.
    *
    * scala> parseLog("I,147,mice in the air")
    * > KnownLog(Info, 147, "mice in the air")
    *
    * scala> parseLog("E,2,148,weird")
    * > KnownLog(Error(2), 148, "weird")
    *
    * scala> parseLog("X blblbaaaaa")
    * > UnknownLog("X blblbaaaaa")
    *
    * Here is the beginning of one possible approach.
    * */
  def parseLog(str: String): LogMessage = {
    val fields = str.split(",").toList
    val optLog: Option[LogMessage] = fields match {
      case List("I", timestampStr, message) => for {
        timestamp <- parseIntOption(timestampStr)
      } yield KnownLog(Info, timestamp, message)
      case List("W", time, message) => for {
        timestamp <- parseIntOption(time)
      } yield KnownLog(Warning, timestamp, message)
      case List("E", severity, time, message) => for {
        severity <- parseIntOption(severity)
        timestamp <- parseIntOption(time)
      } yield KnownLog(Error(severity), timestamp, message)
      case List(meg) if !meg.isBlank => Some(UnknownLog(meg))
      case _ => Some(MalformedLog)
    }

    // What should we do if optLog is None?
    optLog match {
      case Some(KnownLog(logLevel, time, msg)) => KnownLog(logLevel, time, msg)
      case Some(UnknownLog(msg)) => UnknownLog(msg)
      case _ => MalformedLog
    }
  }

  /**
    * scala> parseLogFile("I,147,mice in the air\nX blblbaaaaa")
    * > List(KnownLog(Info, 147, "mice in the air"), UnknownLog("X blblbaaaaa"))
    *
    * Hint: Use `parseLog`
    * Hint: Convert an Array to a List with .toList
    * What if we get an empty line from the fileContent?
    */
  def parseLogFile(fileContent: String): List[LogMessage] = {
    fileContent.split('\n').toList.map(l => parseLog(l)).collect({
      case KnownLog(logLevel, time, msg) => KnownLog(logLevel, time, msg)
      case UnknownLog(message) => UnknownLog(message)
    })
  }

  /**
    * Define a function that returns only logs that are errors over the given severity level.
    * It should not return any log that is not an error.
    *
    * scala> getErrorsOverSeverity(List(KnownLog(Error(2), 123, some error msg"), UnknownLog("blblbaaaaa")), 1)
    * > List(KnownLog(Error(2), 123, some error msg"))
    *
    * scala> getErrorsOverSeverity(List(KnownLog(Error(2), 123, some error msg")), 2)
    * > List()
    * */
  def getErrorsOverSeverity(logs: List[LogMessage], minimumSeverity: Int): List[LogMessage] = logs.collect({
    case KnownLog(logLevel: Error, time, msg) if logLevel.severity > minimumSeverity => KnownLog(logLevel, time, msg)
  })

  /**
    * Write a function to convert a `LogMessage` to a readable `String`.
    *
    * scala> showLogMessage(KnownLog(Info, 147, "mice in the air"))
    * > "Info (147) mice in the air"
    *
    * scala> showLogMessage(KnownLog(Error(2), 147, "weird"))
    * > "Error 2 (147) weird"
    *
    * scala> showLogMessage(UnknownLog("message"))
    * > "Unknown log: message"
    *
    * Hint: Pattern match and use string interpolation
    * */
  def showLogMessage(log: LogMessage): String = log match {
    case KnownLog(logLevel, timestamp, msg) => logLevel match {
      case Error(s) => s"Error $s ($timestamp) $msg"
      case Info => s"Info ($timestamp) $msg"
      case Warning => s"Warning ($timestamp) $msg"
    }
    case UnknownLog(msg) => s"Unknown log: $msg"
    case MalformedLog => s"Malformed Log"
  }

  /**
    * Use `showLogMessage` on error logs with severity greater than the given `severity`.
    *
    * scala> showErrorsOverSeverity(logFile, 2)
    * > List("Error 5 (158) some strange error")
    *
    * Hint: Use `parseLogFile`, `getErrorsOverSeverity` and `showLogMessage`
    * */
  def showErrorsOverSeverity(fileContent: String, severity: Int): List[String] = for {
    lines <- getErrorsOverSeverity(parseLogFile(fileContent), severity)
  } yield showLogMessage(lines)

  /**
    * Now head over to `Main.scala` in the same package to complete the rest of the program.
    */

}
