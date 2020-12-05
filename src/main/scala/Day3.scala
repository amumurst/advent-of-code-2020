package no.amumurst

object Day3 extends DayRunner(3) {
  def slopeCheck(file: List[String], xSkip: Int, ySkip: Int): Int =
    file
      .grouped(ySkip)
      .flatMap(_.headOption)
      .foldLeft((0, 0)) {
        case ((pos, n), line) =>
          ((pos + xSkip) % line.length, n + line.drop(pos).headOption.count(_ == '#'))
      }
      ._2

  override def runA(file: List[String]): Any =
    slopeCheck(file, 3, 1)

  override def runB(file: List[String]): Any =
    slopeCheck(file, 1, 1).toLong *
      slopeCheck(file, 3, 1).toLong *
      slopeCheck(file, 5, 1).toLong *
      slopeCheck(file, 7, 1).toLong *
      slopeCheck(file, 1, 2).toLong
}
