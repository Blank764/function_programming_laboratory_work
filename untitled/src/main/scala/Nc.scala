import better.files.File

case class Nc() extends SystemUtility {
  val name: String = "Netcat"
  val executable: String = "nc"
  val configSection: String = "nc"
  def parseForConsole(fullOutput: String): String = {
    if fullOutput.contains("succeeded") || fullOutput.contains("open") then
      "Порт 443 открыт"
    else
      "Порт 443 закрыт"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Netcat ===\n$fullOutput")
  }
}
