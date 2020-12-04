package no.amumurst

object Days {
  val allDays: List[DayRunner] = List(
    Day1,
    Day2,
    Day3,
    Day4
  )

  def runDays(task: DayTask): Unit =
    allDays.filter(_.isForDay(task)).foreach(_.run(task))
}
