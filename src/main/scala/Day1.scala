package no.amumurst

object Day1 extends DayRunner(1) {
  def recursN(ns: List[List[Int]], sumSoFar: Int = 0, multi: Int = 1): Option[Int] =
    ns match {
      case Nil              => None
      case innerList :: Nil => innerList.find(_ + sumSoFar == 2020).map(_ * multi)
      case firstList :: otherLists =>
        firstList.view.map(i => recursN(otherLists, i + sumSoFar, i * multi)).collectFirst { case Some(value) => value }
    }

  override def runA(file: List[String]): Any = {
    val fileInts = file.flatMap(_.toIntOption)

    recursN(List.fill(2)(fileInts))
  }

  override def runB(file: List[String]): Any = {
    val fileInts = file.flatMap(_.toIntOption)

    recursN(List.fill(3)(fileInts))
  }

  /*
   Short but non-optimized answer
    for {
      a <- fileInts
      b <- fileInts
      c <- fileInts
      if a + b + c == 2020
    } yield a * b * c
 */
}
