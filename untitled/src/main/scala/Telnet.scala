import better.files.File

case class Telnet() extends SystemUtility {
  val name: String = "Telnet"
  val executable: String = "telnet"
  val configSection: String = "telnet"
  def parseForConsole(fullOutput: String): String = {
    if fullOutput.contains("Connected") then
      "Порт 80 открыт"
    else if fullOutput.contains("timeout") || fullOutput.contains("refused") then
      "Порт 80 закрыт"
    else
      "Статус неизвестен"
  }

  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== Telnet ===\n$fullOutput")
  }
}