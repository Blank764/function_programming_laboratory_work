import better.files.File

case class Nslookup() extends SystemUtility {
  val name: String = "Nslookup"
  val executable: String = "nslookup"
  val configSection: String = "nslookup"
  def parseForConsole(fullOutput: String): String = {
    val ips = fullOutput.linesIterator
      .filter(_.contains("Address:"))
      .map(_.split("Address:").last.trim)
      .filter(_.matches("\\d+\\.\\d+\\.\\d+\\.\\d+"))
      .toList

    if ips.nonEmpty then
      s"DNS google.com: ${ips.distinct.mkString(", ")}"
    else
      "DNS не ответил"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Nslookup ===\n$fullOutput")
  }
}
