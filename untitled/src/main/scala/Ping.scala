import better.files.File

case class Ping() extends SystemUtility {
  val name: String = "Ping"
  val executable: String = "ping"
  val configSection: String = "ping"
  def parseForConsole(fullOutput: String): String = {
    val stats = fullOutput.linesIterator
      .find(_.contains("packet loss"))
      .getOrElse("Нет статистики")

    val avgTime = fullOutput.linesIterator
      .find(_.contains("rtt"))
      .map(line =>
        line.split("/").lift(4).getOrElse("?").trim + " ms"
      )
      .getOrElse("")

    s"$stats${if avgTime.nonEmpty then s", среднее: $avgTime" else ""}"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Ping ===\n$fullOutput")
  }
}