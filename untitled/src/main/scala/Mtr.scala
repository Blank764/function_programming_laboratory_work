import better.files.File

case class Mtr() extends SystemUtility {
  val name: String = "MTR"
  val executable: String = "mtr"
  val configSection: String = "mtr"
  def parseForConsole(fullOutput: String): String = {
    val loss = fullOutput.linesIterator
      .find(_.contains("8.8.8.8"))
      .map(line =>
        line.split("\\s+").lift(3).getOrElse("?") + "% loss"
      )
      .getOrElse("?")

    s"Потери: $loss"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== MTR ===\n$fullOutput")
  }
}
