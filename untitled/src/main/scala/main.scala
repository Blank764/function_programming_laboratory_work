import ParseError.EmptyInput
import better.files.*
import org.ini4j.*
import java.io.FileReader
import java.util.UUID
import scala.io.AnsiColor.*
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try, Using}

trait MenuNode:
  def show: String

case class MenuLeaf(name: String, target: Option[Menu]) extends MenuNode:
  override def show: String = name

case class MenuContainer2(name: String, content: Seq[MenuNode]) extends MenuNode:
  override def show: String = name

def menuLoop(current: MenuContainer2, parents: List[MenuContainer2], handler: AbstractHandler, inputData: List[List[String]]): Unit =
  val parentOpt = parents.headOption
  val content = parentOpt match
    case Some(p) => current.content.appended(MenuLeaf("Назад", Some(Menu.NavigateBack)))
    case None    => current.content
  content
    .zipWithIndex
    .map(p => s"${p._2}. ${p._1.show}")
    .foreach(println)
  val rawInput = StdIn.readLine()
  parseUserInput(rawInput, inSubMenu = parents.nonEmpty) match
    case Left(error) =>
      val errorMsg = error match
        case ParseError.EmptyInput => "Ввод не может быть пустым"
        case ParseError.NotANumber(str) => s"'$str' не является числом"
        case ParseError.UnknownCommand(num) => s"Команда '$num' не существует"
      println(s"${RED}Ошибка: $errorMsg${RESET}")
      menuLoop(current, parents, handler, inputData)

    case Right(menuCommand) =>
      menuCommand match
        case Menu.NavigateToPartial =>
          val subMenus = current.content.collect { case mc: MenuContainer2 => mc }
          if subMenus.nonEmpty then
            menuLoop(subMenus.head, current :: parents, handler, inputData)
          else
            println("Подменю не найдено")
            menuLoop(current, parents, handler, inputData)
        case Menu.NavigateBack =>
          parents match
            case p :: ps => menuLoop(p, ps, handler, inputData)
            case Nil     => menuLoop(current, parents, handler, inputData)
        case Menu.Exit =>
          println("Завершение работы...")
          sys.exit(0)
        case _ =>
          executeCommand(menuCommand, handler, inputData)
          menuLoop(current, parents, handler, inputData)

def executeCommand(command: Menu, handler: AbstractHandler, inputData: List[List[String]]): Unit =
  command match
    case Menu.FullDiagnostic =>
      full_diagnostic(handler, inputData)
    case Menu.Exit =>
      sys.exit(0)
    case Menu.CmpReport =>
      compare()
    case Menu.Report =>
      if GlobalConfig.lastDiagnosticResults.isEmpty then
        println("Сначала запустите полную или частичную диагностику.")
      else
        Proper.printReport(GlobalConfig.lastDiagnosticResults)
    case Menu.Diagnostic(target) =>
      part_diagnostic(handler, inputData, target)

def readCheckedInt(): Int =
  Try(StdIn.readLine().trim.toInt) match
    case Failure(_) =>
      println("Пожалуйста, введите число")
      readCheckedInt()
    case Success(value) =>
      value

def compare():Unit = {
  print("Введите UUID первой сессии: ")
  val first = StdIn.readLine()
  print("Введите UUID второй сессии: ")
  val second = StdIn.readLine()
  val content: Try[String] = Using(Source.fromFile(first+".log", "UTF-8")) { source =>
    source.mkString
  }
  content match {
    case Success(content) => {
      println(s"${RED}Сессия ${first}: ${RESET}")
      println(content)
    }
    case Failure(ex)      => println(s"Не удалось открыть файл: ${ex.getMessage}")
  }
  val content2: Try[String] = Using(Source.fromFile(second + ".log", "UTF-8")) { source =>
    source.mkString
  }
  content2 match {
    case Success(content) => {
      println(s"${RED}Сессия ${second}: ${RESET}")
      println(content)
    }
    case Failure(ex) => println(s"Не удалось открыть файл: ${ex.getMessage}")
  }
}

def findAppPrint(all: List[String], need: List[String]): List[String] = {
  need.filter { needed =>
    val isFound = all.exists(fileName => fileName == needed)
    if isFound then
      println(s"${GREEN}[+]${RESET} ${needed}")
    else {
      println(s"${RED}[-]${RESET} ${needed}")
    }
    isFound
  }
}

def full_diagnostic(hand: AbstractHandler, input: List[List[String]]): Unit =
  println("Запуск полной диагностики...")
  val results: List[DiagnosticResult] = hand.handle(input)
  GlobalConfig.lastDiagnosticResults = results
  results.foreach { res =>
    println(s"\n--- ${res.targetName} (${res.utilityName}) ---")
    res.outcome match {
      case Right(text) => println(s"${GREEN}[+] $text${RESET}")
      case Left(err)   => println(s"${RED}[-] Ошибка: $err${RESET}")
    }
  }
  Proper.printReport(results)

def part_diagnostic(hand: AbstractHandler, input: List[List[String]], target: DiagnosticTarget): Unit =
  val targetId = target match {
    case DiagnosticTarget.Interface   => "1"
    case DiagnosticTarget.Gate        => "2"
    case DiagnosticTarget.InetNode    => "3"
    case DiagnosticTarget.DNS         => "4"
    case DiagnosticTarget.WebResource => "5"
    case DiagnosticTarget.PathtoNode  => "6"
    case DiagnosticTarget.Ping        => "7"
  }
  val filteredInput = input.filter(_.headOption.contains(targetId))
  if filteredInput.isEmpty then
    println(s"${RED}Нет данных для проверки: ${target}${RESET}")
  else
    println(s"Запуск проверки: ${target}...")
    val results: List[DiagnosticResult] = hand.handle(filteredInput)
    results.foreach { res =>
      println(s"\n--- ${res.targetName} (${res.utilityName}) ---")
      res.outcome match {
        case Right(text) => println(s"${GREEN}[+] $text${RESET}")
        case Left(err)   => println(s"${RED}[-] Ошибка: $err${RESET}")
      }
    }
    Proper.printReport(results)


def parseUserInput(input: String, inSubMenu: Boolean): Either[ParseError, Menu] =
  input.trim match
    case "" => Left(ParseError.EmptyInput)
    case str if inSubMenu =>
      str.toIntOption match
        case Some(0) => Right(Menu.Diagnostic(DiagnosticTarget.Interface))
        case Some(1) => Right(Menu.Diagnostic(DiagnosticTarget.Gate))
        case Some(2) => Right(Menu.Diagnostic(DiagnosticTarget.InetNode))
        case Some(3) => Right(Menu.Diagnostic(DiagnosticTarget.DNS))
        case Some(4) => Right(Menu.Diagnostic(DiagnosticTarget.WebResource))
        case Some(5) => Right(Menu.Diagnostic(DiagnosticTarget.PathtoNode))
        case Some(6) => Right(Menu.Diagnostic(DiagnosticTarget.Ping))
        case Some(7) => Right(Menu.NavigateBack)
        case _       => Left(ParseError.UnknownCommand(str.toInt))

    case "0" => Right(Menu.FullDiagnostic)
    case "1" => Right(Menu.Exit)
    case "2" => Right(Menu.CmpReport)
    case "3" => Right(Menu.Report)
    case "4" => Right(Menu.NavigateToPartial)
    case str =>
      str.toIntOption match
        case Some(num) => Left(ParseError.UnknownCommand(num))
        case None      => Left(ParseError.NotANumber(str))

object GlobalConfig {
  var conf_file: String = ""
  var output_file: String = ""
  var lastDiagnosticResults: List[DiagnosticResult] = List.empty
}

def interfaceFactory(name: String): Option[SystemUtility] = name match
  case "ip" => Some(IpLink())
  case "ifconfig" => Some(Ifconfig())
  case _ => None

def traceFactory(name: String): Option[SystemUtility] = name match
  case "ip" => Some(IpRoute())
  case "traceroute" => Some(Traceroute())
  case "ping" => Some(Ping())
  case _ => None

def nodeFactory(name: String): Option[SystemUtility] = name match
  case "ping" => Some(Ping())
  case _ => None

def dnsFactory(name: String): Option[SystemUtility] = name match
  case "dig" => Some(Dig())
  case "host" => Some(Host())
  case _ => None

def webFactory(name: String): Option[SystemUtility] = name match
  case "curl" => Some(Curl())
  case "wget" => Some(Wget())
  case "telnet" => Some(Telnet())
  case "nc" => Some(Nc())
  case _ => None

def pathFactory(name: String): Option[SystemUtility] = name match
  case "traceroute" => Some(Traceroute())
  case "mtr" => Some(Mtr())
  case "tracepath" => Some(Tracepath())
  case _ => None

def pingFactory(name: String): Option[SystemUtility] = name match
  case "ping" => Some(Ping())
  case "mtr" => Some(Mtr())
  case "iperf3" => Some(Iperf3())
  case _ => None

@main def main(config_file: String): Unit = {
  println(s"***************************************")
  println(s"****program present by Gimik Dmitry****")
  println(s"***********linux edition***************")
  val sessionUuid: String = UUID.randomUUID().toString

  val ini_reader = new Ini(new FileReader(config_file))

  GlobalConfig.conf_file = config_file
  GlobalConfig.output_file = sessionUuid + ".log"

  val root = ini_reader.get("bin", "path")

  val interface = ini_reader.get("bin", "interface").split(",").map(_.trim.toString).toList
  val trace = ini_reader.get("bin", "trace").split(",").map(_.trim.toString).toList
  val node_ip = ini_reader.get("bin", "node_ip").split(",").map(_.trim.toString).toList
  val DNS = ini_reader.get("bin", "DNS").split(",").map(_.trim.toString).toList
  val web_resource = ini_reader.get("bin", "web_resource").split(",").map(_.trim.toString).toList
  val path_to_node = ini_reader.get("bin", "path_to_node").split(",").map(_.trim.toString).toList
  val ping = ini_reader.get("bin", "ping").split(",").map(_.trim.toString).toList

  val progs = File(root).list.map(_.name).toList

  val findInterface = "1"::findAppPrint(progs, interface)
  val findTrace = "2"::findAppPrint(progs, trace)
  val findNode_ip = "3"::findAppPrint(progs, node_ip)
  val findDNS = "4"::findAppPrint(progs, DNS)
  val findWeb_resource = "5"::findAppPrint(progs, web_resource)
  val findPath_to_node = "6"::findAppPrint(progs, path_to_node)
  val findPing = "7"::findAppPrint(progs, ping)

  val inp = List(findInterface,findTrace,findNode_ip,findDNS,findWeb_resource,
    findPath_to_node,findPing)

  val pa = GenericHandler("7", "Ping", pingFactory)
  val pn = GenericHandler("6", "Маршрут", pathFactory).setNext(pa)
  val we = GenericHandler("5", "Web-ресурс", webFactory).setNext(pn)
  val dn = GenericHandler("4", "DNS", dnsFactory).setNext(we)
  val nd = GenericHandler("3", "Узел", nodeFactory).setNext(dn)
  val tr = GenericHandler("2", "Шлюз", traceFactory).setNext(nd)
  val in = GenericHandler("1", "Интерфейс", interfaceFactory).setNext(tr)
  val em = GenericHandler("0", "Пусто", _ => None).setNext(in)

  val chain = em

  val container = MenuContainer2(
    "Главное меню",
    Seq(
      MenuLeaf("Полная диагностика", Some(Menu.FullDiagnostic)),
      MenuLeaf("Выход", Some(Menu.Exit)),
      MenuLeaf("Сравнение сессий", Some(Menu.CmpReport)),
      MenuLeaf("Отчет", Some(Menu.Report)),
      MenuContainer2(
        "Частичная диагностика",
        Seq(
          MenuLeaf("Проверка сетевого интерфейса", Some(Menu.Diagnostic(DiagnosticTarget.Interface))),
          MenuLeaf("Проверка доступности локального шлюза", Some(Menu.Diagnostic(DiagnosticTarget.Gate))),
          MenuLeaf("Доступность выбранного узла в интернете", Some(Menu.Diagnostic(DiagnosticTarget.InetNode))),
          MenuLeaf("Проверка DNS", Some(Menu.Diagnostic(DiagnosticTarget.DNS))),
          MenuLeaf("Проверка доступности web-ресурса", Some(Menu.Diagnostic(DiagnosticTarget.WebResource))),
          MenuLeaf("Проверить маршрут до выбранного узла", Some(Menu.Diagnostic(DiagnosticTarget.PathtoNode))),
          MenuLeaf("Проверить задержку и потери пакетов", Some(Menu.Diagnostic(DiagnosticTarget.Ping))),
        )
      )
    )
  )

  Try(menuLoop(container, Nil, chain, inp)) match
    case Failure(exception) => println(s"Критическая ошибка: ${exception.getMessage}")
    case Success(_) => println("Программа завершена корректно")
}

