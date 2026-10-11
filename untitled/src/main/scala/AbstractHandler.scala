trait AbstractHandler {
  def handle(spisok: List[List[String]]): Unit
  def setNext(nextHandler: AbstractHandler): AbstractHandler
}