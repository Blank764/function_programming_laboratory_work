import sys.process.*
import com.typesafe.scalalogging.LazyLogging
import org.ini4j.Ini

import java.io.FileReader

case class UtilityResult(
                          success: Boolean,
                          exitCode: Int,
                          consoleOutput: String,
                          fullOutput: String,
                          errors: List[String] = Nil
                        )

trait SystemUtility extends LazyLogging {
  val config_file: String = GlobalConfig.conf_file
  val name: String
  val executable: String
  val configSection: String

  val ini_reader = new Ini(new FileReader(config_file))

  def run(): List[UtilityResult] ={
    val args = ini_reader.get(configSection, "arg").split(",").toList
    var list_results:List[UtilityResult] = List()
    args.foreach { arg =>
      val command = executable + " " + arg
      logger.info(s"Выполняю команду: ${command.mkString(" ")}")
      try {
        val fullOutput = command.!!
        logger.info(s"Команда выполнена: ${command.mkString(" ")}")
        val consoleSummary = parseForConsole(fullOutput)
        list_results = list_results.appended(
        UtilityResult(
          success = true,
          exitCode = 0,
          consoleOutput = consoleSummary,
          fullOutput = fullOutput
        ))
      } catch {
        case e: Exception =>
          logger.error(s"Ошибка при выполнении '${command.mkString(" ")}': ${e.getMessage}")
          list_results = list_results.appended(
          UtilityResult(
            success = false,
            exitCode = -1,
            consoleOutput = s"Ошибка запуска: ${e.getMessage}",
            fullOutput = e.getMessage,
            errors = List(e.getMessage)
          ))
      }
    }
    list_results
  }
  def parseForConsole(fullOutput: String): String
  def insertToLogFile(fullOutput: String, pathToFile: String): Unit
}