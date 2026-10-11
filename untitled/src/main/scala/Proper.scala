object Proper {
  var interface = -1
  var trace = -1
  var node_ip = -1
  var DNS = -1
  var web_resource = -1
  var path_to_node = -1
  var ping = -1
  var resultString = ""

  def setExit(num : String, util: String , exit: UtilityResult) : Unit ={
    num match {
      case "1" => {
        interface = exit.exitCode
        if exit.exitCode !=0 then
          resultString+="Не установлен сетевой интерфейс\n"
        else if exit.exitCode == 0 then
          resultString += "Сетевой интерфейс успешно найден\n"
      }
      case "2" => {
        if util =="traceroute" then {
          resultString+=parseTracerouteOutput(exit.fullOutput, exit.exitCode)+"\n"
          return
        }
        if exit.exitCode!=0 then {
          resultString += "Локальный шлюз не доступен\n"
        }else {
          resultString += "Локальный шлюз найден\n"
        }
        trace = exit.exitCode
      }
      case "3" => {
        if exit.exitCode != 0 then {
          resultString += "Сетевой узел не доступен\n"
        } else {
          resultString += "Сетевой узел доступен\n"
        }
        node_ip = exit.exitCode
      }
      case "4" => {
        if util == "dig" then {
          resultString += parseDigOutput(exit.fullOutput, exit.exitCode) + "\n"
          return
        }
        if util == "nslookup" && exit.fullOutput.contains("NXDOMAIN") then {
          resultString += "Домен не найден\n"
          return
        }
        if exit.exitCode !=0 then
          resultString += "Домен не найден\n"
        else
          resultString += "Домен найден\n"
        DNS = exit.exitCode
      }
      case "5" => {
        if util == "curl" then
          web_resource = exit.exitCode
          resultString += parseCurlOutput(exit.fullOutput, exit.exitCode) + "\n"
          return
        if exit.exitCode!=0 then
          resultString += "Web-ресурс не доступен\n"
        else
          resultString += "Web-ресурс доступен\n"
        web_resource = exit.exitCode
      }
      case "6" => {
        if util == "traceroute" then
          resultString += parseTracerouteOutput(exit.fullOutput, exit.exitCode) + "\n"
          return
        if util == "mtr" then
          resultString += parseMtrOutput(exit.fullOutput, exit.exitCode) + "\n"
          return
        if util == "tracepath" then
          path_to_node = exit.exitCode
          resultString += parseTracepathOutput(exit.fullOutput, exit.exitCode) + "\n"
          return
      }
      case "7" => {
        if util == "mtr" then
          resultString += parseMtrOutput(exit.fullOutput, exit.exitCode) + "\n"
          return
        if exit.exitCode != 0 then
          resultString += "Потери пакетов 100%\n"
        else
          resultString += "Ответные пакеты получены\n"
        ping = exit.exitCode
      }
      }
  }
  def PrintReport():Unit={
    println(resultString)
    println(predict(interface, trace, node_ip, DNS, web_resource, path_to_node, ping))
  }

  def parseTracerouteOutput(output: String, exitCode: Int): String = {
    if (exitCode != 0) {
      if (output.contains("name resolution") ||
        output.contains("Name or service not known") ||
        output.contains("unknown host")) {
        path_to_node = 1
        return "Ошибка DNS: хост не найден"
      }
      if (output.contains("Network is unreachable") ||
        output.contains("No route to host")) {
        path_to_node = 1
        return "Сеть недоступна: нет маршрута до хоста"
      }
      return s"Неизвестная ошибка (код $exitCode): $output"
    }
    val lines = output.split("\n").toList
    val lostHops = lines.count(_.trim.matches("""\d+\s+\*\s+\*\s+\*.*"""))
    val totalHops = lines.count(_.trim.matches("""\d+\s+.*"""))

    if (totalHops == 0) {
      return "Трассировка не дала результатов"
    }
    val lastLine = lines.lastOption.getOrElse("")
    val reachedDestination = !lastLine.contains("* * *")
    path_to_node= 0
    if (reachedDestination && lostHops == 0) {
      return s"Маршрут найден: $totalHops переходов, потерь нет"
    } else if (reachedDestination) {
      return s"Маршрут найден: $totalHops переходов, потерь: $lostHops"
    } else {
      return s"Маршрут не завершён: $totalHops переходов, потерь: $lostHops"
    }
  }

  def parseDigOutput(output: String, exitCode: Int): String = {

    if (output.contains("connection timed out") ||
      output.contains("communications error")) {
      DNS =1
      return "Ошибка сети: таймаут подключения к DNS-серверу"
    }

    if (output.contains("REFUSED")) {
      DNS = 1
      return "DNS-сервер отказал в обслуживании"
    }

    if (output.contains("SERVFAIL")) {
      DNS = 1
      return "Ошибка DNS-сервера: SERVFAIL"
    }

    if (output.contains("NXDOMAIN")) {
      DNS = 1
      return "Домен не найден: NXDOMAIN"
    }

    if (output.contains("NOERROR")) {
      if (output.contains("ANSWER SECTION:")) {
        return "Домен найден, записи получены"
      } else {
        return "Домен существует, но запрошенных записей нет"
      }
    }

    if (output.contains("status:")) {
      return s"Неизвестный статус DNS: ${output.split("status:")(1).split(",")(0).trim}"
    }

    if (exitCode != 0) {
      return s"Критическая ошибка dig (код $exitCode): $output"
    }
    DNS = 0
    "DNS-запрос выполнен успешно"
  }

  def parseCurlOutput(output: String, exitCode: Int): String = {
    if (exitCode == 22) {
      return "HTTP ошибка (4xx или 5xx): ресурс недоступен"
    }

    if (exitCode == 6) {
      return "Ошибка DNS: не удалось разрешить имя хоста"
    }

    if (exitCode == 7) {
      return "Ошибка подключения: отказ в соединении (connection refused)"
    }

    if (exitCode == 28) {
      return "Таймаут: сервер не ответил вовремя"
    }

    if (exitCode == 35) {
      return "Ошибка SSL/TLS: проблема с сертификатом или шифрованием"
    }

    if (exitCode != 0) {
      return s"Неизвестная ошибка curl (код $exitCode): $output"
    }

    if (output.contains("HTTP/1.1 200") || output.contains("HTTP/2 200")) {
      return "HTTP 200 OK: ресурс доступен"
    }

    if (output.contains("HTTP/1.1 301") || output.contains("HTTP/1.1 302")) {
      return "️Редирект (301/302): ресурс перемещён"
    }

    if (output.contains("HTTP/1.1 404")) {
      return "HTTP 404: ресурс не найден"
    }

    if (output.contains("HTTP/1.1 500") || output.contains("HTTP/1.1 502") ||
      output.contains("HTTP/1.1 503")) {
      return "HTTP ошибка сервера (5xx): сервер недоступен"
    }

    if (output.contains("Could not resolve host")) {
      return "Ошибка DNS: хост не найден"
    }

    if (output.contains("Connection refused")) {
      return "Порт закрыт: соединение отклонено"
    }

    if (output.contains("Timed out")) {
      return "Таймаут подключения"
    }

    "Запрос выполнен (статус не определён)"
  }

  def parseMtrOutput(output: String, exitCode: Int): String = {
    if (exitCode != 0) {
      path_to_node = exitCode
      if (output.contains("Cannot resolve") || output.contains("No such host")) {
        return "Ошибка DNS: хост не найден"
      }
      if (output.contains("permission denied") || output.contains("Operation not permitted")) {
        return "Ошибка прав доступа: нужны права root"
      }
      return s"Критическая ошибка mtr (код $exitCode): $output"
    }

    val lines = output.split("\n").map(_.trim).filter(_.nonEmpty).toList

    if (lines.isEmpty) {
      return "Вывод mtr пуст"
    }
    val targetLine = lines.last
    val lossPattern = """(\d+\.?\d*)\s*%""".r

    lossPattern.findFirstMatchIn(targetLine) match {
      case Some(matchResult) =>
        val loss = matchResult.group(1).toDouble

        if (loss == 0.0) {
          "Маршрут найден: потерь пакетов нет"
        } else if (loss < 20.0) {
          s"Маршрут найден, но есть небольшие потери: ${loss}%"
        } else if (loss < 50.0) {
          s"Маршрут найден, высокие потери пакетов: ${loss}%"
        } else {
          s"Критические потери пакетов: ${loss}%"
        }

      case None =>
        if (targetLine.contains("???")) {
          "Хосты не отвечают: маршрут не прослежен"
        } else {
          s"Не удалось определить потери пакетов. Последняя строка: '$targetLine'"
        }
    }
  }

  def parseTracepathOutput(output: String, exitCode: Int): String = {
    if (exitCode != 0) {
      path_to_node = exitCode
      if (output.contains("Name or service not known") ||
        output.contains("unknown host")) {
        return " Ошибка DNS: хост не найден"
      }
      if (output.contains("Network is unreachable")) {
        return "Сеть недоступна"
      }
      if (output.contains("Permission denied")) {
        return "Ошибка прав доступа"
      }
      return s"Ошибка tracepath (код $exitCode): $output"
    }
    val lines = output.split("\n").toList
    val noReplyCount = lines.count(_.contains("no reply"))
    val totalHops = lines.count(_.trim.matches("""\d+:.*"""))

    if (totalHops == 0) {
      return "Трассировка не дала результатов"
    }
    path_to_node = exitCode
    val lastLine = lines.lastOption.getOrElse("")
    val reachedDestination = lastLine.contains("reached") ||
      (!lastLine.contains("no reply") && lastLine.contains(":"))

    if (reachedDestination && noReplyCount == 0) {
      return s"Маршрут найден: $totalHops хопов, потерь нет"
    } else if (reachedDestination) {
      return s"️Маршрут найден: $totalHops хопов, потерь: $noReplyCount"
    } else {
      return s"️Маршрут не завершён: $totalHops хопов, потерь: $noReplyCount"
    }
  }

    def predict(
                 interfaceState: Int,
                 traceState: Int,
                 nodeIpState: Int,
                 dnsState: Int,
                 webResourceState: Int,
                 pathToNodeState: Int,
                 pingState: Int
               ): String = {

      def isOk(state: Int): Boolean = state == 0

      def isFailed(state: Int): Boolean = state == 1

      def isUnstable(state: Int): Boolean = state == 2

      def isChecked(state: Int): Boolean = state != -1

      if (isFailed(interfaceState)) {
        return
          """Предполагаемая проблема: локальная сетевая конфигурация.
            |На активном интерфейсе не обнаружен IP-адрес или маршрут по умолчанию.
            |Рекомендация: Проверьте настройки сетевого адаптера или подключение кабеля/Wi-Fi.""".stripMargin
      }
      if (isChecked(pingState) && isUnstable(pingState)) {
        return
          """Предполагаемая проблема: нестабильное сетевое соединение.
            |Превышен порог потерь пакетов или критическая задержка.
            |Рекомендация: Проверьте качество сигнала Wi-Fi, загрузку канала или обратитесь к провайдеру.""".stripMargin
      }
      if (isOk(interfaceState) && isFailed(nodeIpState)) {
        return
          """Предполагаемая проблема: недоступен локальный шлюз.
            |Интерфейс настроен, но шлюз не отвечает на запросы.
            |Возможные причины: физическое отключение, проблема с маршрутизатором, неверный IP шлюза.""".stripMargin
      }
      if (isOk(nodeIpState) && isFailed(pathToNodeState) && isFailed(dnsState) && isFailed(webResourceState)) {
        return
          """Предполагаемая проблема: есть локальная сеть, но нет доступа в интернет.
            |Локальный шлюз доступен, но внешние узлы, DNS и HTTP-проверки не проходят.
            |Возможные причины: проблемы у интернет-провайдера, блокировка на уровне провайдера, неоплаченный тариф.""".stripMargin
      }
      if (isOk(nodeIpState) && isOk(pathToNodeState) && isFailed(dnsState) && isFailed(webResourceState)) {
        return
          """Предполагаемая проблема: сбой в работе DNS.
            |Сетевое подключение и доступ к внешним IP-адресам работают, но доменные имена не разрешаются.
            |Рекомендация: Попробуйте сменить DNS-серверы на публичные (например, 8.8.8.8 или 1.1.1.1).""".stripMargin
      }
      if (isOk(nodeIpState) && isOk(pathToNodeState) && isOk(dnsState) && isFailed(webResourceState)) {
        return
          """Предполагаемая проблема: недоступность конкретного веб-ресурса.
            |Общее сетевое подключение и разрешение имён работают корректно.
            |Возможные причины: сайт временно не работает, блокировка конкретного ресурса, проблемы на стороне сервера.""".stripMargin
      }
      if (isOk(interfaceState) && isOk(nodeIpState) && isOk(pathToNodeState) && isOk(dnsState) && isOk(webResourceState) && isOk(pingState)) {
        return "Сетевое подключение работает корректно. Критических ошибок не обнаружено."
      }
      """Проблема не определена.
        |Результаты проверок противоречивы или недостаточны для точного заключения.
        |Рекомендуется изучить полные логи выполнения утилит (fullOutput) вручную.""".stripMargin
    }

}
