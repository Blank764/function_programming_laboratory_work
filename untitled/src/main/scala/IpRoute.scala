import better.files.File

case class IpRoute() extends SystemUtility {
  val name: String = "IP Route"
  val executable: String = "ip"
  val configSection: String = "ip route"
  def parseForConsole(fullOutput: String): String = {
    val defaultRoute = fullOutput.linesIterator
      .find(_.startsWith("default"))
      .map(line => 
        line.split("\\s+").lift(2).getOrElse("не найден")
      )
    
    defaultRoute match {
      case Some(gateway) => s"Шлюз по умолчанию: $gateway"
      case None => "Маршрут по умолчанию не найден"
    }
  }
  
  def insertToLogFile(fullOutput: String, pathToFile: String): Unit = {
    File(pathToFile).appendLine(s"\n=== IP Route ===\n$fullOutput")
  }
}
