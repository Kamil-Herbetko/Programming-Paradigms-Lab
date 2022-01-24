package Lista12


object Lista12 {
  val sem: MySemaphore = new MySemaphore(1)
  val intCell: IntCell = new IntCell

  class IntCell {
    private var n: Int = 0

    def getN(): Int =
      try {
        sem.acquire(2)
      } catch {
        case e: InterruptedException => println(e)
      }
      n

    def setN(newN: Int): Unit =
      n = newN
      sem.release(2)
  }

  class Count extends Thread {
    override def run(): Unit =
      for (i <- 0 to 200000)
        var temp: Int = intCell.getN()
        intCell.setN(temp + 1)
  }


  def main(args: Array[String]): Unit = {
    val p: Count = new Count()
    val q: Count = new Count()
    p.start()
    q.start()

    try {p.join(); q.join()}
    catch {
      case e: InterruptedException => println(e)
    }
    println("The value of n is " + intCell.getN())
  }

}
