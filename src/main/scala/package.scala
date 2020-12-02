package no

import scala.util.ChainingSyntax

package object amumurst extends ChainingSyntax {
  implicit class AnyStuff[A](a: => A) {
    def time(): A = {
      val start  = System.currentTimeMillis()
      val result = a
      System
        .currentTimeMillis()
        .pipe(stop => s"Time used: ${stop - start}ms")
        .tap(println)
        .pipe(_ => result)
    }
  }

  def readFile(d: DayTask): List[String] =
    scala.io.Source.fromResource(s"Day${d.day}.txt").getLines().toList

  abstract class DayRunner {
    def runA(file: List[String]): Any
    def runB(file: List[String]): Any

    def isForDay(d: DayTask): Boolean

    def run(d: DayTask): Unit = d.task match {
      case TaskA => runA(readFile(d)).pipe(println).time()
      case TaskB => runB(readFile(d)).pipe(println).time()
    }
  }

  sealed trait Task extends Serializable with Product
  object Task {
    def unapply(c: Char): Option[Task] = c match {
      case 'a' => Some(TaskA)
      case 'b' => Some(TaskB)
      case _   => None
    }
  }
  case object TaskA extends Task
  case object TaskB extends Task

  case class DayTask(day: Int, task: Task)

  object DayTask {
    def unapply(s: String): Option[DayTask] =
      s.toList match {
        case a :: Task(b) :: Nil =>
          a.toString.toIntOption.map(i => DayTask(i, b))
        case _ => None
      }
  }

}
