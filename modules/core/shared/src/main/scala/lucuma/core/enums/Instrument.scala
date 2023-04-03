// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for instruments.
 *
 * @group Enumerations
 */
sealed abstract class Instrument(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val obsolete:  Boolean
) extends Product with Serializable

object Instrument {

  /** @group Constructors */ case object AcqCam     extends Instrument("AcqCam", "AcqCam", "Acquisition Camera", false)
  /** @group Constructors */ case object Bhros      extends Instrument("Bhros", "bHROS", "bHROS", true)
  /** @group Constructors */ case object Flamingos2 extends Instrument("Flamingos2", "Flamingos2", "Flamingos 2", false)
  /** @group Constructors */ case object Ghost      extends Instrument("Ghost", "GHOST", "GHOST", false)
  /** @group Constructors */ case object GmosNorth  extends Instrument("GmosNorth", "GMOS-N", "GMOS North", false)
  /** @group Constructors */ case object GmosSouth  extends Instrument("GmosSouth", "GMOS-S", "GMOS South", false)
  /** @group Constructors */ case object Gnirs      extends Instrument("Gnirs", "GNIRS", "GNIRS", false)
  /** @group Constructors */ case object Gpi        extends Instrument("Gpi", "GPI", "GPI", false)
  /** @group Constructors */ case object Gsaoi      extends Instrument("Gsaoi", "GSAOI", "GSAOI", false)
  /** @group Constructors */ case object Igrins2    extends Instrument("Igrins2", "IGRINS2", "IGRINS2", false)
  /** @group Constructors */ case object Michelle   extends Instrument("Michelle", "Michelle", "Michelle", false)
  /** @group Constructors */ case object Nici       extends Instrument("Nici", "NICI", "NICI", false)
  /** @group Constructors */ case object Nifs       extends Instrument("Nifs", "NIFS", "NIFS", false)
  /** @group Constructors */ case object Niri       extends Instrument("Niri", "NIRI", "NIRI", false)
  /** @group Constructors */ case object Phoenix    extends Instrument("Phoenix", "Phoenix", "Phoenix", false)
  /** @group Constructors */ case object Trecs      extends Instrument("Trecs", "TReCS", "TReCS", false)
  /** @group Constructors */ case object Visitor    extends Instrument("Visitor", "Visitor Instrument", "Visitor Instrument", false)
  /** @group Constructors */ case object Scorpio    extends Instrument("Scorpio", "SCORPIO", "Scorpio", false)
  /** @group Constructors */ case object Alopeke    extends Instrument("Alopeke", "ALOPEKE", "Alopeke", false)
  /** @group Constructors */ case object Zorro      extends Instrument("Zorro", "ZORRO", "Zorro", false)

  /** All members of Instrument, in canonical order. */
  val all: List[Instrument] =
    List(
      AcqCam,
      Bhros,
      Flamingos2,
      Ghost,
      GmosNorth,
      GmosSouth,
      Gnirs,
      Gpi,
      Gsaoi,
      Igrins2,
      Michelle,
      Nici,
      Nifs,
      Niri,
      Phoenix,
      Trecs,
      Visitor,
      Scorpio,
      Alopeke,
      Zorro
    )

  /** Select the member of Instrument with the given tag, if any. */
  def fromTag(s: String): Option[Instrument] =
    all.find(_.tag === s)

  /** Select the member of Instrument with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): Instrument =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"Instrument: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val InstrumentEnumerated: Enumerated[Instrument] =
    new Enumerated[Instrument] {
      def all: List[Instrument] =
        Instrument.all

      def tag(a: Instrument): String =
        a.tag

      override def unsafeFromTag(s: String): Instrument =
        Instrument.unsafeFromTag(s)
    }

}
