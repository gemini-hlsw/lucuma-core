// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Enumerated
import lucuma.refined.*

/**
 * Enumerated type for instruments.
 *
 * @group Enumerations
 */
sealed abstract class Instrument(
  val tag:           String,
  val shortName:     String,
  val longName:      String,
  val referenceName: NonEmptyString,
  val obsolete:      Boolean
) extends Product with Serializable

object Instrument {

  /** @group Constructors */ case object AcqCam     extends Instrument("AcqCam", "AcqCam", "Acquisition Camera", "ACQCAM".refined[NonEmpty], false)
  /** @group Constructors */ case object Bhros      extends Instrument("Bhros", "bHROS", "bHROS", "BHROS".refined[NonEmpty], true)
  /** @group Constructors */ case object Flamingos2 extends Instrument("Flamingos2", "Flamingos2", "Flamingos 2", "F2".refined[NonEmpty], false)
  /** @group Constructors */ case object Ghost      extends Instrument("Ghost", "GHOST", "GHOST", "GHOST".refined[NonEmpty], false)
  /** @group Constructors */ case object GmosNorth  extends Instrument("GmosNorth", "GMOS-N", "GMOS North", "GMOSN".refined[NonEmpty], false)
  /** @group Constructors */ case object GmosSouth  extends Instrument("GmosSouth", "GMOS-S", "GMOS South", "GMOSS".refined[NonEmpty], false)
  /** @group Constructors */ case object Gnirs      extends Instrument("Gnirs", "GNIRS", "GNIRS", "GNIRS".refined[NonEmpty], false)
  /** @group Constructors */ case object Gpi        extends Instrument("Gpi", "GPI", "GPI", "GPI".refined[NonEmpty], false)
  /** @group Constructors */ case object Gsaoi      extends Instrument("Gsaoi", "GSAOI", "GSAOI", "GSAOI".refined[NonEmpty], false)
  /** @group Constructors */ case object Igrins2    extends Instrument("Igrins2", "IGRINS2", "IGRINS2", "IGRINS2".refined[NonEmpty], false)
  /** @group Constructors */ case object Michelle   extends Instrument("Michelle", "Michelle", "Michelle", "MICHELLE".refined[NonEmpty], false)
  /** @group Constructors */ case object Nici       extends Instrument("Nici", "NICI", "NICI", "NICI".refined[NonEmpty], false)
  /** @group Constructors */ case object Nifs       extends Instrument("Nifs", "NIFS", "NIFS", "NIFS".refined[NonEmpty], false)
  /** @group Constructors */ case object Niri       extends Instrument("Niri", "NIRI", "NIRI", "NIRI".refined[NonEmpty], false)
  /** @group Constructors */ case object Phoenix    extends Instrument("Phoenix", "Phoenix", "Phoenix", "PHOENIX".refined[NonEmpty], false)
  /** @group Constructors */ case object Trecs      extends Instrument("Trecs", "TReCS", "TReCS", "TRECS".refined[NonEmpty], false)
  /** @group Constructors */ case object Visitor    extends Instrument("Visitor", "Visitor Instrument", "Visitor Instrument", "VISITOR".refined[NonEmpty], false)
  /** @group Constructors */ case object Scorpio    extends Instrument("Scorpio", "SCORPIO", "Scorpio", "SCORPIO".refined[NonEmpty], false)
  /** @group Constructors */ case object Alopeke    extends Instrument("Alopeke", "ALOPEKE", "Alopeke", "ALOPEKE".refined[NonEmpty], false)
  /** @group Constructors */ case object Zorro      extends Instrument("Zorro", "ZORRO", "Zorro", "ZORRO".refined[NonEmpty], false)

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
