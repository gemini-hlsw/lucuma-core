// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration unit continuum lamps.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalContinuum(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GcalContinuum {

  /** @group Constructors */ case object IrGreyBodyLow extends GcalContinuum("IrGreyBodyLow", "IR grey body - low", "IR grey body - low", false)
  /** @group Constructors */ case object IrGreyBodyHigh extends GcalContinuum("IrGreyBodyHigh", "IR grey body - high", "IR grey body - high", false)
  /** @group Constructors */ case object QuartzHalogen5W extends GcalContinuum("QuartzHalogen5", "5W Quartz Halogen", "5W Quartz Halogen", false)
  /** @group Constructors */ case object QuartzHalogen100W extends GcalContinuum("QuartzHalogen100", "100W Quartz Halogen", "100W Quartz Halogen", false)

  /** All members of GcalContinuum, in canonical order. */
  val all: List[GcalContinuum] =
    List(IrGreyBodyLow, IrGreyBodyHigh, QuartzHalogen5W, QuartzHalogen100W)

  /** Select the member of GcalContinuum with the given tag, if any. */
  def fromTag(s: String): Option[GcalContinuum] =
    all.find(_.tag === s)

  /** Select the member of GcalContinuum with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GcalContinuum =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GcalContinuum: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GcalContinuumEnumerated: Enumerated[GcalContinuum] =
    new Enumerated[GcalContinuum] {
      def all = GcalContinuum.all
      def tag(a: GcalContinuum) = a.tag
      override def unsafeFromTag(s: String): GcalContinuum =
        GcalContinuum.unsafeFromTag(s)
    }

}
