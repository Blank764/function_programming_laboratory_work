import better.files.File

case class Dig() extends SystemUtility {
  val name: String = "Dig"
  val executable: String = "dig"
  val configSection: String = "dig"
  def parseForConsole(fullOutput: String): String = {
    val ips = fullOutput.linesIterator
      .filter(_.matches("\\d+\\.\\d+\\.\\d+\\.\\d+"))
      .toList
      .distinct

    if ips.nonEmpty then
      s"DNS: ${ips.mkString(", ")}"
    else
      "DNS не ответил"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Dig ===\n$fullOutput")
  }
}