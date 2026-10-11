import better.files.File

case class Traceroute() extends SystemUtility {
  val name: String = "Traceroute"
  val executable: String = "traceroute"
  val configSection: String = "traceroute"
  def parseForConsole(fullOutput: String): String = {
    val hops = fullOutput.linesIterator.count(line =>
      line.matches("^\\s*\\d+\\s+.*") && !line.contains("traceroute")
    )

    val reachedDestination = fullOutput.contains("8.8.8.8")

    if reachedDestination then
      s"Маршрут до 8.8.8.8: $hops хопов"
    else
      s"Маршрут прервался на $hops хопе"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Traceroute ===\n$fullOutput")
  }
}
