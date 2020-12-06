package no.amumurst

object Day6 extends DayRunner(6) {
  def countUnique(l: List[String]): Int = l.flatMap(_.toList).distinct.size
  def countAll(l: List[String]): Int    = l.flatMap(_.distinct.toList).groupBy(identity).values.count(_.size == l.size)

  def getGroups(l: List[String]): List[List[String]] =
    l.map(s => if (s.isEmpty) "|" else s)
      .mkString(" ")
      .split('|')
      .toList
      .map(_.trim)
      .map(_.split(' ').toList)

  override def runA(file: List[String]): Any =
    file
      .pipe(getGroups)
      .map(countUnique)
      .sum

  override def runB(file: List[String]): Any =
    file
      .pipe(getGroups)
      .map(countAll)
      .sum
}
