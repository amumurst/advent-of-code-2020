package no.amumurst

object Day5 extends DayRunner(5) {
  def binaryLoop[A](right: A, left: A)(start: Int, end: Int)(bs: List[A]): Int = {
    @scala.annotation.tailrec
    def inner(cLeft: Int, cRight: Int, bs: List[A]): Int =
      bs.headOption match {
        case Some(value) if value == right => inner(cLeft, (cLeft + cRight) / 2, bs.tail)
        case Some(value) if value == left  => inner((cLeft + cRight + 1) / 2, cRight, bs.tail)
        case _                             => cLeft
      }
    inner(start, end, bs)
  }

  def calcSeatId(l: String): Int = {
    val row = binaryLoop('F', 'B')(0, 127)(l.toList.take(7))
    val col = binaryLoop('L', 'R')(0, 7)(l.toList.drop(7))
    row * 8 + col
  }

  override def runA(file: List[String]): Any =
    file.map(calcSeatId).max

  override def runB(file: List[String]): Any =
    file.map(calcSeatId).sorted.sliding(3).toList.filter {
      case a :: b :: c :: Nil => (a + c - 2 * b) != 0
    }
}
