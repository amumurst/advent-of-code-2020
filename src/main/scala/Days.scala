package no.amumurst

object Days {
  val allDays: List[DayRunner] = List(
    Day1,
    Day2,
    Day3,
    Day4
  )

  def runDays(task: DayTask): Unit =
    allDays.filter(_.day == task.day).foreach(_.run(task))
}
