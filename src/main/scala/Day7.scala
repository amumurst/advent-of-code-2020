package no.amumurst

object Day7 extends DayRunner(7) {
  case class BagName(s: String)
  object BagName{
    def unapply(s: String): Option[BagName] = s match {
      case s"$name bag$_" => Some(BagName(name))
      case _ => None
    }
  }

  case class BagList(l: List[BagName]){
    def grouped: Map[BagName, Int] = l.groupBy(identity).view.mapValues(_.size).toMap
  }
  object BagList{
    def fromString(s: String): BagList =
      s.split(",").toList.flatMap {
        case s" no other bags." => List.empty
        case s" $n ${BagName(name)}" => List.fill(n.toInt)(name)
        case s" $n ${BagName(name)}." => List.fill(n.toInt)(name)
        case _ => List.empty
      }.pipe(BagList.apply)
  }

  case class BagRule(name: BagName, contains: BagList)

  def fromString(s: String): Option[BagRule] = s.split("contain").toList match {
    case ::(BagName(head), next) => Some(BagRule(head, BagList.fromString(next.mkString)))
    case _ => None
  }


  def backwards(l: List[BagRule], lookFor: List[BagName], bags: Set[BagName] = Set.empty):Int =
    l.filter(_.contains.l.exists(lookFor.contains)).map(_.name) match {
      case Nil => bags.size
      case found => backwards(l, found, bags ++ found.toSet)
    }

  def forwards(l: List[BagRule], lookFor: BagName):Int =
    l.filter(_.name == lookFor) match {
      case Nil => 0
      case ls => 1 + ls.flatMap(rule => rule.contains.grouped.map{case (k, v) => v * forwards(l, k)}).sum
    }

  override def runA(file: List[String]): Any =
    file.flatMap(fromString).pipe(backwards(_, List(BagName("shiny gold"))))

  override def runB(file: List[String]): Any =
    file.flatMap(fromString).pipe(forwards(_, BagName("shiny gold"))).pipe(_ - 1)
}
