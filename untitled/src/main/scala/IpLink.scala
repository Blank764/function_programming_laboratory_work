import better.files.File

case class IpLink() extends SystemUtility {
  val name: String = "IP Link"
  val executable: String = "ip"
  val configSection: String = "ip link"
  def parseForConsole(fullOutput: String): String = {
    val interfaces = fullOutput.linesIterator
      .filter(_.contains(": "))
      .map(line => {
        val parts = line.split(": ")
        if parts.length >= 2 then parts(1).trim
        else ""
      })
      .filter(_.nonEmpty)
      .toList

    if interfaces.nonEmpty then
      s"Интерфейсы: ${interfaces.mkString(", ")}"
    else
      "Интерфейсы не найдены"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== IP Link ===\n$fullOutput")
  }
}