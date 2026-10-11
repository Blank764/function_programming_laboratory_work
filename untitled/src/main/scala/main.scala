import better.files.*
import org.ini4j.*

import java.io.FileReader
import java.util.UUID
import scala.io.AnsiColor.*
import scala.io.{Source, StdIn}
import scala.util.{Try, Using, Success, Failure}

trait MenuNode:
  def show: String

case class MenuLeaf(name: String, action: () => Unit) extends MenuNode:
  override def show: String = name

case class MenuContainer2(name: String, content: Seq[MenuNode]) extends MenuNode:
  override def show: String = name


def menuLoop(current: MenuContainer2, parents: List[MenuContainer2]): Unit =
  val parentOpt = parents.headOption

  val content = parentOpt match
    case Some(p) => current.content.appended(p)
    case None    => current.content

  content
    .zipWithIndex
    .map(p => s"${p._2}. ${p._1.show}")
    .foreach(println)

  val choice = readCheckedInt()

  if parentOpt.isDefined && choice == current.content.length then
    menuLoop(parentOpt.get, parents.tail)
  else if choice >= 0 && choice < current.content.length then
    content(choice) match
      case leaf: MenuLeaf =>
        leaf.action()
        menuLoop(current, parents)
      case mc: MenuContainer2 =>
        menuLoop(mc, current :: parents)
  else
    println("Неверный выбор, попробуйте снова")
    menuLoop(current, parents)


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
      println(s"${RED}Сессия ${first}: ${RESET}")
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

def full_diagnostic(hand: AbstractHandler, input: List[List[String]]):Unit = {
  println("полная диагностика")
  hand.handle(input)
}

def part_diagnostic(hand: AbstractHandler, input: List[List[String]], command: String):Unit = {3
  try{
    val selectedIds: Set[String] = command.split("\\s+").map(_.trim).toSet
    val filteredInput: List[List[String]] = input.filter { innerList =>
      val id = innerList.headOption.getOrElse("")
      selectedIds.contains(id)
    }
    hand.handle(filteredInput)
  } catch {
    case e: NumberFormatException=>println("некорректный ввод. повторите попытку")
  }
  finally {
    return
  }
}

object GlobalConfig {
  var conf_file :String = ""
  var output_file :String = ""
}

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

  val findInterface = "1"::findAppPrint(progs, interface);
  val findTrace = "2"::findAppPrint(progs, trace);
  val findNode_ip = "3"::findAppPrint(progs, node_ip);
  val findDNS = "4"::findAppPrint(progs, DNS);
  val findWeb_resource = "5"::findAppPrint(progs, web_resource);
  val findPath_to_node = "6"::findAppPrint(progs, path_to_node);
  val findPing = "7"::findAppPrint(progs, ping);

  val inp = List(findInterface,findTrace,findNode_ip,findDNS,findWeb_resource,
    findPath_to_node,findPing)

  var run : Boolean = true


  val pa = PingApp()
  val pn = PathToNodeHandler().setNext(pa)
  val we = WebResHandler().setNext(pn)
  val dn = DNSHandler().setNext(we)
  val nd = NodeHandler().setNext(dn)
  val tr = TraceHandle().setNext(nd)
  val in = InterfaceHandler().setNext(tr)
  val em = EmptyHandler().setNext(in)

  val chain = em
  val ml1 = MenuLeaf(s" полная диагностика", ()=>full_diagnostic(chain,inp))
  val ml2 = MenuLeaf(s" выход", ()=>sys.exit(0))
  val ml3 = MenuLeaf(s" сравнение сессий", ()=>compare())
  val ml4 = MenuLeaf(s" отчет", ()=>Proper.PrintReport())

  val container = MenuContainer2(
    "Главное меню",
    Seq(
      ml1, ml2, ml3, ml4,
      MenuContainer2(
        "частичная диагностика",
        Seq(
          MenuLeaf(" проверка сетевого интерфейса ", () => part_diagnostic(chain,inp, "1")),
          MenuLeaf(" проверка доступность локального шлюза ", () => part_diagnostic(chain,inp, "2")),
          MenuLeaf(" доступность выбранного узла в интернете ", () => part_diagnostic(chain,inp, "3")),
          MenuLeaf(" проверка DNS", () => part_diagnostic(chain,inp, "4")),
          MenuLeaf(" проверка доступности web-ресурса ", () => part_diagnostic(chain,inp, "5")),
          MenuLeaf(" проверить маршрут до выбранного узла ", () => part_diagnostic(chain,inp, "6")),
          MenuLeaf(" проверить задержку и потери пакетов", () => part_diagnostic(chain,inp, "7"))
        )
      )
    )
  )
  Try(menuLoop(container, Nil)) match
    case Failure(exception) => println("Something has failed successfully")
    case Success(_) => println("Something has failed to fail")
}

