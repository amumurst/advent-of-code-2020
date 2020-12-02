package no.amumurst

object Main {
  def main(args: Array[String]): Unit =
    args.headOption
      .flatMap(DayTask.unapply)
      .fold(println(s"Could not find task ${args.headOption}"))(Days.runDays)
}
