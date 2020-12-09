package no.amumurst

import scala.annotation.tailrec

object Day9 extends DayRunner(9) {
  implicit class Combs(ls: Seq[Long]) {
    def canSumTo(i: Long): Boolean = ls.flatMap(x => ls.map(_ + x)).contains(i)
  }

  def findWeakness(file: LazyList[Long], preamb: Int): Option[Long] =
    file
      .sliding(preamb + 1)
      .drop(preamb)
      .find(a => !a.take(preamb).canSumTo(a.last))
      .flatMap(_.lastOption)

  //258585477
  override def runA(file: List[String]): Any =
    findWeakness(LazyList.from(file.map(_.toLong)), 25)

  //36981213
  override def runB(file: List[String]): Any = {
    val longs    = LazyList.from(file.map(_.toLong))
    val weakness = runA(file).asInstanceOf[Option[Long]].get

    @tailrec
    def loop(all: LazyList[Long], curr: List[Long] = Nil, s: Long = 0): List[Long] =
      if (s == weakness) curr
      else
        all.headOption match {
          case Some(head) => loop(all.tail, head +: curr, s + head)
          case None       => Nil
        }

    (0 to file.size)
      .pipe(LazyList.from[Int])
      .map(i => longs.drop(i))
      .map(loop(_))
      .find(_.nonEmpty)
      .map(_.pipe(p => p.max + p.min))
  }
}
