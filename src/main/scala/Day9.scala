package no.amumurst

import scala.annotation.tailrec

object Day9 extends DayRunner(9) {
  def sums(s: LazyList[Long]): LazyList[Long] = s.flatMap(x => s.map(_ + x))

  def findWeakness(file: LazyList[Long], preamb: Int): Option[Long] =
    file
      .sliding(preamb + 1)
      .drop(preamb)
      .find(a => !sums(a.take(preamb)).contains(a.last))
      .flatMap(_.lastOption)

  @tailrec
  def findSummable(all: LazyList[Long], searchFor: Long, curr: List[Long] = Nil, s: Long = 0): List[Long] =
    if (s == searchFor) curr
    else
      all.headOption match {
        case Some(head) => findSummable(all.tail, searchFor, head +: curr, s + head)
        case None       => Nil
      }

  override def runA(file: List[String]): Any =
    findWeakness(LazyList.from(file.map(_.toLong)), 25)

  override def runB(file: List[String]): Any = {
    val input = LazyList.from(file.map(_.toLong))

    val weakness = findWeakness(input, 25).get
    LazyList
      .from[Int](0 to file.size)
      .map(input.drop)
      .map(findSummable(_, weakness))
      .find(_.nonEmpty)
      .map(p => p.max + p.min)
  }
}
