enum DiagnosticTarget:
  case Interface, Gate, InetNode, DNS, WebResource, PathtoNode, Ping

enum Menu:
  case Exit
  case FullDiagnostic
  case CmpReport
  case Report
  case Diagnostic(target: DiagnosticTarget)
  case NavigateToPartial
  case NavigateBack

enum ParseError:
  case EmptyInput
  case NotANumber(input: String)
  case UnknownCommand(number: Int)

enum DiagnosticError:
  case NoProgramsConfigured(target: String)
  case UtilityNotSupported(name: String)
  case ExecutionFailed(exitCode: Int, message: String)
  case UtilityNotFound(name: String)
