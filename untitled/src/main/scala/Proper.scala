import scala.io.AnsiColor.*

object Proper {
  def generateReport(results: List[DiagnosticResult]): String = {
    def isFailed(target: String): Boolean =
      results.find(_.targetName == target).exists(_.outcome.isLeft)

    def isSuccess(target: String): Boolean =
      results.find(_.targetName == target).exists(_.outcome.isRight)

    def isChecked(target: String): Boolean =
      results.exists(_.targetName == target)

    if isFailed("Интерфейс") then
      return """Предполагаемая проблема: локальная сетевая конфигурация.
               |На активном интерфейсе не обнаружен IP-адрес или маршрут по умолчанию.
               |Рекомендация: Проверьте настройки сетевого адаптера или подключение кабеля/Wi-Fi.""".stripMargin

    if isChecked("Ping") && results.find(_.targetName == "Ping").exists(_.outcome match {
      case Left(err) => err.toString.contains("нестабильное") || err.toString.contains("потери")
      case _ => false
    }) then
      return """Предполагаемая проблема: нестабильное сетевое соединение.
               |Превышен порог потерь пакетов или критическая задержка.
               |Рекомендация: Проверьте качество сигнала Wi-Fi или обратитесь к провайдеру.""".stripMargin

    if isSuccess("Интерфейс") && isFailed("Шлюз") then
      return """Предполагаемая проблема: недоступен локальный шлюз.
               |Интерфейс настроен, но шлюз не отвечает на запросы.
               |Возможные причины: физическое отключение, проблема с маршрутизатором.""".stripMargin

    if isSuccess("Шлюз") && isFailed("Маршрут") && isFailed("DNS") && isFailed("Web-ресурс") then
      return """Предполагаемая проблема: есть локальная сеть, но нет доступа в интернет.
               |Локальный шлюз доступен, но внешние узлы, DNS и HTTP-проверки не проходят.
               |Возможные причины: проблемы у интернет-провайдера или неоплаченный тариф.""".stripMargin

    if isSuccess("Шлюз") && isSuccess("Маршрут") && isFailed("DNS") && isFailed("Web-ресурс") then
      return """Предполагаемая проблема: сбой в работе DNS.
               |Сетевое подключение и доступ к внешним IP-адресам работают, но доменные имена не разрешаются.
               |Рекомендация: Попробуйте сменить DNS-серверы на публичные (8.8.8.8 или 1.1.1.1).""".stripMargin

    if isSuccess("Шлюз") && isSuccess("Маршрут") && isSuccess("DNS") && isFailed("Web-ресурс") then
      return """Предполагаемая проблема: недоступность конкретного веб-ресурса.
               |Общее сетевое подключение и разрешение имён работают корректно.
               |Возможные причины: сайт временно не работает или заблокирован.""".stripMargin

    if isSuccess("Интерфейс") && isSuccess("Шлюз") && isSuccess("Маршрут") && isSuccess("DNS") && isSuccess("Web-ресурс") && isSuccess("Ping") then
      return "Сетевое подключение работает корректно. Критических ошибок не обнаружено."

    """Проблема не определена.
      |Результаты проверок противоречивы или недостаточны для точного заключения.
      |Рекомендуется изучить полные логи выполнения утилит вручную.""".stripMargin
  }
  def printReport(results: List[DiagnosticResult]): Unit = {
    println(s"\n${YELLOW}=== Детальный отчет о проверках ===${RESET}")

    if results.isEmpty then
      println("Нет данных для отображения. Запустите диагностику.")
    else
      results.foreach { res =>
        val statusIcon = res.outcome match {
          case Right(_) => s"${GREEN}[+]${RESET}"
          case Left(_)  => s"${RED}[-]${RESET}"
        }
        val statusText = res.outcome match {
          case Right(msg) => msg
          case Left(err)  => err.toString
        }
        println(s"$statusIcon ${res.targetName} (${res.utilityName}): $statusText")
      }
    println(s"\n${BLUE}=== Итоговый диагноз ===${RESET}")
    println(generateReport(results))
  }
}