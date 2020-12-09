package no.amumurst

object Days {
  val allDays: List[DayRunner] = List(
    Day1,
    Day2,
    Day3,
    Day4,
    Day5,
    Day6,
    Day7,
    Day9,
  )

  def runDays(task: DayTask): Unit =
    allDays.filter(_.day == task.day).foreach(_.run(task))
}
