// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for "smart" calibration sequence tpes.
 * @group Enumerations (Generated)
 */
sealed abstract class SmartGcalType(
  val tag: String
) extends Product
    with Serializable {

  def fold[X](lamp: GcalLampType => X, baseline: GcalBaselineType => X): X =
    this match {
      case SmartGcalType.Arc           => lamp(GcalLampType.Arc)
      case SmartGcalType.Flat          => lamp(GcalLampType.Flat)
      case SmartGcalType.DayBaseline   => baseline(GcalBaselineType.Day)
      case SmartGcalType.NightBaseline => baseline(GcalBaselineType.Night)
    }

}

object SmartGcalType {

  /** @group Constructors */
  case object Arc extends SmartGcalType("Arc")

  /** @group Constructors */
  case object Flat extends SmartGcalType("Flat")

  /** @group Constructors */
  case object DayBaseline extends SmartGcalType("DayBaseline")

  /** @group Constructors */
  case object NightBaseline extends SmartGcalType("NightBaseline")

  /** All members of SmartGcalType, in canonical order. */
  val all: List[SmartGcalType] =
    List(Arc, Flat, DayBaseline, NightBaseline)

  /** Select the member of SmartGcalType with the given tag, if any. */
  def fromTag(s: String): Option[SmartGcalType] =
    all.find(_.tag === s)

  /** Select the member of SmartGcalType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): SmartGcalType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"SmartGcalType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val SmartGcalTypeEnumerated: Enumerated[SmartGcalType] =
    new Enumerated[SmartGcalType] {
      def all = SmartGcalType.all
      def tag(a:                    SmartGcalType)         = a.tag
      override def unsafeFromTag(s: String): SmartGcalType =
        SmartGcalType.unsafeFromTag(s)
    }

}
