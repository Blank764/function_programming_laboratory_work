case class WebResHandler(next: Option[AbstractHandler] = None) extends AbstractHandler {

  override def handle(spisok: List[List[String]]): Unit = {
    val findInterface = spisok.filter(x => x(0)=="5").flatten
    if findInterface.isEmpty then
      next.foreach(_.handle(spisok))
      return
    if findInterface.length<2 then
      println("К сожалению провести проверку доступа к внешним ресурсам невозможно так как не установлены программы проверки в конфигурационном файле")
    else
      val Interface = findInterface(1)
      val prog:SystemUtility = Interface match {
        case "curl" => Curl()
        case "wget" => Wget()
        case "telnet" => Telnet()
        case "nc" => Nc()
        case _ => {
          println("программа не поддерживается приложением")
          null
        }
      }
      if prog!=null then
        val result: List[UtilityResult] = prog.run()
        result.foreach{ res => 
          println(s"${prog.parseForConsole(res.fullOutput)}")
          prog.insertToLogFile(res.fullOutput,GlobalConfig.output_file)
          Proper.setExit(findInterface(0),findInterface(1),res)
        }
        
    next.foreach(_.handle(spisok))
  }

  override def setNext(nextHandler: AbstractHandler): AbstractHandler = {
    this.copy(next = Some(nextHandler))
  }
}