package no.amumurst

object Day2 extends DayRunner {
  trait PWChecker {
    def check(s: String): Boolean
    def &&(other: PWChecker): PWChecker = (s: String) => this.check(s) && other.check(s)
    def ||(other: PWChecker): PWChecker = (s: String) => this.check(s) || other.check(s)
    def ^(other: PWChecker): PWChecker  = (s: String) => this.check(s) ^ other.check(s)
    def ! : PWChecker                   = (s: String) => !this.check(s)
  }

  case class MaxMinRule(letter: Char, min: Int, max: Int) extends PWChecker {
    override def check(s: String): Boolean =
      Option(s.count(_ == letter)).filter(_ >= min).exists(_ <= max)
  }
  object MaxMinRule {
    def unapply(s: String): Option[MaxMinRule] =
      s match {
        case s"$min-$max $char" => Some(MaxMinRule(char.head, min.toInt, max.toInt))
        case _                  => None
      }
  }
  case class AtPositionRule(letter: Char, pos: Int) extends PWChecker {
    override def check(s: String): Boolean = s.take(pos).lastOption.contains(letter)
  }

  case class ExactPositionRule(letter: Char, first: Int, last: Int) extends PWChecker {
    override def check(s: String): Boolean =
      (AtPositionRule(letter, first) ^ AtPositionRule(letter, last)).check(s)
  }
  object ExactPositionRule {
    def unapply(s: String): Option[ExactPositionRule] =
      s match {
        case s"$first-$last $char" => Some(ExactPositionRule(char.head, first.toInt, last.toInt))
        case _                     => None
      }
  }
  override def runA(file: List[String]): Any =
    file.collect { case s"${MaxMinRule(a)}: $word" if a.check(word) => 1 }.size

  override def runB(file: List[String]): Any =
    file.collect { case s"${ExactPositionRule(a)}: $word" if a.check(word) => 1 }.size

  override def isForDay(d: DayTask): Boolean = d.day == 2
}
