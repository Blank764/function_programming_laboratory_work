import better.files.File

case class Tracepath() extends SystemUtility {
  val name: String = "Tracepath"
  val executable: String = "tracepath"
  val configSection: String = "tracepath"
  def parseForConsole(fullOutput: String): String = {
    val hops = fullOutput.linesIterator.count(line =>
      line.matches("^\\s*\\d+:.*")
    )

    s"До 8.8.8.8: $hops хопов"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Tracepath ===\n$fullOutput")
  }
}