// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.internal.WitnessAs
import eu.timepit.refined.numeric.Interval
import lucuma.core.optics.SplitEpi
import monocle.Focus
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ElevationRange extends Product with Serializable

object ElevationRange {

  final case class AirMass private (
    min: AirMass.DecimalValue,
    max: AirMass.DecimalValue
  ) extends ElevationRange

  final case class HourAngle private (
    minHours: HourAngle.DecimalHour,
    maxHours: HourAngle.DecimalHour
  ) extends ElevationRange

  object AirMass {
    val MinValue = BigDecimal(1.0)
    given WitnessAs[MinValue.type, BigDecimal] = WitnessAs(MinValue, MinValue)
    val MaxValue = BigDecimal(3.0)
    given WitnessAs[MaxValue.type, BigDecimal] = WitnessAs(MaxValue, MaxValue)

    type Value        = Interval.Closed[MinValue.type, MaxValue.type]
    type DecimalValue = BigDecimal Refined Value
    object DecimalValue extends RefinedTypeOps[DecimalValue, BigDecimal]

    val DefaultMin: DecimalValue = DecimalValue.unsafeFrom(BigDecimal(1.0))
    val DefaultMax: DecimalValue = DecimalValue.unsafeFrom(BigDecimal(2.0))
    val Default: AirMass         = apply(DefaultMin, DefaultMax)

    /** @group Typeclass Instances */
    implicit val AirMassEq: Eq[AirMass] = Eq.by(ar => (ar.min.value, ar.max.value))

    /** @group Optics */
    val min = Focus[AirMass](_.min)

    /** @group Optics */
    val max = Focus[AirMass](_.max)

    /** @group Optics */
    // Ensures that min <= max by swapping if necessary
    val fromDecimalValues: SplitEpi[(DecimalValue, DecimalValue), AirMass] =
      SplitEpi(
        t => {
          val (min, max) = t
          if (min.value <= max.value) AirMass(min, max)
          else AirMass(max, min)
        },
        a => (a.min, a.max)
      )

    /** @group Optics */
    val fromOrderedDecimalValues: Prism[(DecimalValue, DecimalValue), AirMass] =
      Prism[(DecimalValue, DecimalValue), AirMass] { case (min, max) =>
        Option.when(min.value <= max.value)(AirMass(min, max))
      }(a => (a.min, a.max))
  }

  object HourAngle {
    val MinHour = BigDecimal(-5.0)
    given WitnessAs[MinHour.type, BigDecimal] = WitnessAs(MinHour, MinHour)
    val MaxHour = BigDecimal(5.0)
    given WitnessAs[MaxHour.type, BigDecimal] = WitnessAs(MaxHour, MaxHour)

    type Hour        = Interval.Closed[MinHour.type, MaxHour.type]
    type DecimalHour = BigDecimal Refined Hour
    object DecimalHour extends RefinedTypeOps[DecimalHour, BigDecimal]

    val DefaultMin = DecimalHour.unsafeFrom(MinHour)
    val DefaultMax = DecimalHour.unsafeFrom(MaxHour)

    val Default = HourAngle(DefaultMin, DefaultMax)

    /** @group Typeclass Instances */
    implicit val HourAngleEq: Eq[HourAngle] =
      Eq.by(hr => (hr.minHours.value, hr.maxHours.value))

    /** @group Optics */
    val minHours = Focus[HourAngle](_.minHours)

    /** @group Optics */
    val maxHours = Focus[HourAngle](_.maxHours)

    /** @group Optics */
    // Ensures that minHours <= maxHours by swapping if necessary
    lazy val fromDecimalHours: SplitEpi[(DecimalHour, DecimalHour), HourAngle] =
      SplitEpi(
        t => {
          val (min, max) = t
          if (min.value <= max.value) HourAngle(min, max)
          else HourAngle(max, min)
        },
        a => (a.minHours, a.maxHours)
      )

    /** @group Optics */
    val fromOrderedDecimalHours: Prism[(DecimalHour, DecimalHour), HourAngle] =
      Prism[(DecimalHour, DecimalHour), HourAngle] { case (min, max) =>
        Option.when(min.value <= max.value)(HourAngle(min, max))
      }(a => (a.minHours, a.maxHours))
  }

  /** @group Typeclass Instances */
  implicit val ElevationRangeEq: Eq[ElevationRange] = Eq.fromUniversalEquals

  /** @group Optics */
  val airMass: Prism[ElevationRange, AirMass] =
    GenPrism[ElevationRange, AirMass]

  /** @group Optics */
  val hourAngle: Prism[ElevationRange, HourAngle] =
    GenPrism[ElevationRange, HourAngle]
}
