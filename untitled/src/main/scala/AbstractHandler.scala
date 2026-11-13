trait AbstractHandler:
  def handle(input: List[List[String]]): List[DiagnosticResult]
  def setNext(next: AbstractHandler): AbstractHandler

case class DiagnosticResult(
                             targetName: String,
                             utilityName: String,
                             fullOutput: String,
                             outcome: Either[DiagnosticError, String]
                           )

case class GenericHandler(
                           targetId: String,
                           targetName: String,
                           factory: String => Option[SystemUtility],
                           next: Option[AbstractHandler] = None
                         ) extends AbstractHandler:

  override def handle(input: List[List[String]]): List[DiagnosticResult] =
    val myData = input.find(_.headOption.contains(targetId))

    val myResults: List[DiagnosticResult] = myData match
      case None =>
        List.empty

      case Some(list) if list.length < 2 =>
        List(DiagnosticResult(targetName, "None", "", Left(DiagnosticError.NoProgramsConfigured(targetName))))

      case Some(list) =>
        val progName = list(1)
        factory(progName) match
          case None =>
            List(DiagnosticResult(targetName, progName, "", Left(DiagnosticError.UtilityNotSupported(progName))))
          case Some(prog) =>
            prog.run().map { utilRes =>
              prog.insertToLogFile(utilRes.fullOutput, GlobalConfig.output_file)
              val outcome = if utilRes.success then Right(utilRes.consoleOutput)
              else Left(DiagnosticError.ExecutionFailed(utilRes.exitCode, utilRes.errors.mkString(", ")))
              DiagnosticResult(targetName, progName, utilRes.fullOutput, outcome)
            }
    val nextResults = next.map(_.handle(input)).getOrElse(List.empty)
    myResults ++ nextResults
  override def setNext(nextHandler: AbstractHandler): AbstractHandler =
    this.copy(next = Some(nextHandler))