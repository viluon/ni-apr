package microc.analysis

trait Lattice[E] {
  def top: E
  def bot: E
  def lub(a: E, b: E): E
  def glb(a: E, b: E): E
}

object Lattice {
  implicit class LatOps[E](x: E)(implicit l: Lattice[E]) {
    def ⊔(y: E): E = l.lub(x, y)
    def ⊓(y: E): E = l.glb(x, y)
  }

  sealed trait FlatLat[A]
  object FlatLat {
    case class Top[A]() extends FlatLat[A]
    case class Bot[A]() extends FlatLat[A]
    case class Mid[A](x: A) extends FlatLat[A]
  }

  def flatLat[A]: Lattice[FlatLat[A]] = new Lattice[FlatLat[A]] {
    override val top: FlatLat[A] = FlatLat.Top()
    override val bot: FlatLat[A] = FlatLat.Bot()

    override def lub(a: FlatLat[A], b: FlatLat[A]): FlatLat[A] = a match {
      case top: FlatLat.Top[A] => top
      case mid@FlatLat.Mid(x) => b match {
        case top: FlatLat.Top[A] => top
        case _: FlatLat.Bot[A] => mid
        case FlatLat.Mid(y) if x == y => mid
        case _: FlatLat.Mid[A] => top
      }
      case _: FlatLat.Bot[A] => b
    }

    override def glb(a: FlatLat[A], b: FlatLat[A]): FlatLat[A] = a match {
      case _: FlatLat.Bot[A] => bot
      case mid@FlatLat.Mid(x) => b match {
        case _: FlatLat.Bot[A] => bot
        case _: FlatLat.Top[A] => mid
        case FlatLat.Mid(y) if x == y => mid
        case _: FlatLat.Mid[A] => bot
      }
      case _: FlatLat.Top[A] => b
    }
  }

  def mapLat[A, B](s: Iterable[A], l: Lattice[B]): Lattice[Map[A, B]] = new Lattice[Map[A, B]] {
    private implicit def lat: Lattice[B] = l
    override val top: Map[A, B] = s.map(_ -> l.top).toMap
    override val bot: Map[A, B] = s.map(_ -> l.bot).toMap
    override def lub(a: Map[A, B], b: Map[A, B]): Map[A, B] = a.transform((k, x) => x ⊔ b(k))
    override def glb(a: Map[A, B], b: Map[A, B]): Map[A, B] = a.transform((k, x) => x ⊓ b(k))
  }

  sealed trait LiftLat[A]
  object LiftLat {
    // this is isomorphic to Option, maybe there's a generic
    // lattice-making process for arbitrary ADTs?
    case class Off/* the ground */[A](x: A) extends LiftLat[A]
    case class Bot[A]() extends LiftLat[A]
  }

  def liftLat[A](l: Lattice[A]): Lattice[LiftLat[A]] = new Lattice[LiftLat[A]] {
    override val top: LiftLat[A] = LiftLat.Off(l.top)
    override val bot: LiftLat[A] = LiftLat.Bot()
    override def lub(a: LiftLat[A], b: LiftLat[A]): LiftLat[A] = a match {
      case _: LiftLat.Bot[A] => b
      case off@LiftLat.Off(x) => b match {
        case _: LiftLat.Bot[A] => off
        case LiftLat.Off(y) if x == y => off
        case _: LiftLat.Off[A] => top
      }
    }
    override def glb(a: LiftLat[A], b: LiftLat[A]): LiftLat[A] = a match {
      case bot: LiftLat.Bot[A] => bot
      case off@LiftLat.Off(x) => b match {
        case bot: LiftLat.Bot[A] => bot
        case LiftLat.Off(y) if x == y => off
        case _: LiftLat.Off[A] => bot
      }
    }
  }
}
