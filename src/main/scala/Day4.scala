package no.amumurst

object Day4 extends DayRunner(4) {
  sealed abstract class PassportPart(val name: String) {
    def validateValue(value: String): Boolean
  }
  object PassportPart {
    val required = List(byr, iyr, eyr, hgt, hcl, ecl, pid)
    val optional = List(cid)
    val all      = required ++ optional

    def apply(s: String): Option[PassportPart]   = all.find(_.name == s)
    def unapply(s: String): Option[PassportPart] = apply(s)
  }
  case object byr extends PassportPart("byr") {
    override def validateValue(value: String): Boolean =
      value.toIntOption.filter(_ >= 1920).filter(_ <= 2002).isDefined
  }
  case object iyr extends PassportPart("iyr") {
    override def validateValue(value: String): Boolean =
      value.toIntOption.filter(_ >= 2010).filter(_ <= 2020).isDefined
  }
  case object eyr extends PassportPart("eyr") {
    override def validateValue(value: String): Boolean =
      value.toIntOption.filter(_ >= 2020).filter(_ <= 2030).isDefined
  }
  case object hgt extends PassportPart("hgt") {
    override def validateValue(value: String): Boolean =
      value match {
        case s"${i}in" => i.toIntOption.filter(_ >= 59).filter(_ <= 76).isDefined
        case s"${i}cm" => i.toIntOption.filter(_ >= 150).filter(_ <= 193).isDefined
        case _         => false
      }
  }
  case object hcl extends PassportPart("hcl") {
    object CH {
      def unapply(c: Char): Option[Char] =
        Some(c).filter(c => (c >= 'a' && c <= 'f') || c.isDigit)
    }
    override def validateValue(value: String): Boolean = value.toList match {
      case '#' :: CH(a) :: CH(b) :: CH(c) :: CH(d) :: CH(e) :: CH(f) :: Nil => true
      case _                                                                => false
    }
  }
  case object ecl extends PassportPart("ecl") {
    val allowed = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

    override def validateValue(value: String): Boolean = allowed.contains(value)
  }
  case object pid extends PassportPart("pid") {
    override def validateValue(value: String): Boolean =
      (value.length == 9) && value.forall(_.isDigit)
  }
  case object cid extends PassportPart("cid") {
    override def validateValue(value: String): Boolean = true
  }

  case class PassPortKV(key: PassportPart, v: String) {
    lazy val isValid: Boolean = key.validateValue(v)
  }
  object PassPortKV extends (String => Option[PassPortKV]) {
    def apply(s: String): Option[PassPortKV] = s.split(':').toList match {
      case PassportPart(one) :: two :: Nil => Some(PassPortKV(one, two))
      case _                               => None
    }
  }

  case class PassPortData(ls: List[PassPortKV]) extends AnyVal {
    def checkAllRequired: Boolean = PassportPart.required.forall(p => ls.exists(_.key == p))
    def checkAllData: Boolean     = checkAllRequired && ls.forall(_.isValid)
  }
  object PassPortData {
    def fromStrings(ls: List[String]): List[PassPortData] =
      ls.map(_.trim.split(' ').flatMap(PassPortKV).toList.pipe(PassPortData(_)))
  }

  def parseFile(file: List[String]): List[PassPortData] =
    file
      .map(s => if (s.isEmpty) "|" else s)
      .mkString(" ")
      .split('|')
      .toList
      .pipe(PassPortData.fromStrings)

  override def runA(file: List[String]): Any =
    file
      .pipe(parseFile)
      .count(_.checkAllRequired)
  //264

  override def runB(file: List[String]): Any =
    file
      .pipe(parseFile)
      .count(_.checkAllData)
}
