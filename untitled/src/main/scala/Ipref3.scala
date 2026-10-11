import better.files.File

case class Iperf3() extends SystemUtility {
  val name: String = "Iperf3"
  val executable: String = "iperf3"
  val configSection: String = "iperf3"
  def parseForConsole(fullOutput: String): String = {
    val speed = fullOutput.linesIterator
      .find(_.contains("bits/sec"))
      .map(line =>
        line.split("\\s+").filter(_.contains("bits/sec")).head
      )
      .getOrElse("?")

    s"Скорость: $speed"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Iperf3 ===\n$fullOutput")
  }
}