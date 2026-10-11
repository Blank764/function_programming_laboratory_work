import better.files.File

case class Curl() extends SystemUtility {
  val name: String = "Curl"
  val executable: String = "curl"
  val configSection: String = "curl"
  
  def parseForConsole(fullOutput: String): String = {
    val status = fullOutput.linesIterator
      .find(_.startsWith("HTTP"))
      .map(_.split("\\s+").lift(1).getOrElse("?"))
      .getOrElse("?")

    s"google.com: HTTP $status"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Curl ===\n$fullOutput")
  }
}