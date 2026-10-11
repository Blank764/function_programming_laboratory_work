import better.files.File

case class Ifconfig() extends SystemUtility {
  val name: String = "Ifconfig"
  val executable: String = "ifconfig"
  val configSection: String = "ifconfig"
  def parseForConsole(fullOutput: String): String = {
    val interfaces = fullOutput.linesIterator
      .filter(line => !line.startsWith("\t") && line.matches("\\S+.*"))
      .map(_.split("\\s+").head)
      .filter(_.nonEmpty)
      .toList

    if interfaces.nonEmpty then
      s"Активные интерфейсы: ${interfaces.mkString(", ")}"
    else
      "Нет активных интерфейсов"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Ifconfig ===\n$fullOutput")
  }
}