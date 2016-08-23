package model

object Number extends Enumeration {
  type Number = Value

  val One = Value("1")
  val Two = Value("2")
  val Three = Value("3")
  val Four = Value("4")
  val Five = Value("5")

  val nums = Seq(Number.One,
                 Number.One,
                 Number.One,
                 Number.Two,
                 Number.Two,
                 Number.Three,
                 Number.Three,
                 Number.Four,
                 Number.Four,
                 Number.Five)

  def previous(n : Number) : Option[Number] = {
    n match {
      case Number.One => None
      case i => Some(Number.values.toList(i.id - 1))
    }
  }
}

