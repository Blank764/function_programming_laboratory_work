import better.files.File

case class Host() extends SystemUtility {
  val name: String = "Host"
  val executable: String = "host"
  val configSection: String = "host"
  def parseForConsole(fullOutput: String): String = {
    val ips = fullOutput.linesIterator
      .filter(_.contains("has address"))
      .map(_.split("has address").last.trim)
      .toList

    if ips.nonEmpty then
      s"DNS: ${ips.mkString(", ")}"
    else
      "DNS не ответил"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Host ===\n$fullOutput")
  }
}