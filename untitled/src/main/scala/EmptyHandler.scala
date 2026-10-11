case class EmptyHandler(next: Option[AbstractHandler] = None) extends AbstractHandler {
  override def handle(spisok: List[List[String]]): Unit = {
    if spisok.isEmpty then {
      println("Выбор проверки произошел некорректно. повторите попытку")
      return
    }
    next.foreach(_.handle(spisok))
  }

  override def setNext(nextHandler: AbstractHandler): AbstractHandler = {
    this.copy(next = Some(nextHandler))
  }
}