// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import lucuma.core.math._
import lucuma.core.optics.SplitEpi
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ElevationRange extends Product with Serializable

object ElevationRange {
  val airmass: Prism[ElevationRange, AirmassRange] =
    GenPrism[ElevationRange, AirmassRange]

  val hourAngle: Prism[ElevationRange, HourAngleRange] =
    GenPrism[ElevationRange, HourAngleRange]

  implicit val ElevationRangeEq: Eq[ElevationRange] = Eq.fromUniversalEquals
}

sealed abstract case class AirmassRange protected (
  deciMin: AirmassRange.IntDeciValue,
  deciMax: AirmassRange.IntDeciValue
) extends ElevationRange {
  def min: Double = deciMin.value / 10.0
  def max: Double = deciMax.value / 10.0

  /**
   * Set the min value, and perhaps the max value.
   * Maintains the invariant the min <= max by modifying max to be min if necessary.
   */
  def setMin(deciMin: AirmassRange.IntDeciValue): AirmassRange = {
    val maximum = if (deciMin.value <= this.deciMax.value) this.deciMax else deciMin
    new AirmassRange(deciMin, maximum) {}
  }

  /**
   * Set the max value, and perhaps the min value.
   * Maintains the invariant the min <= max by modifying min to be max if necessary.
   */
  def setMax(deciMax: AirmassRange.IntDeciValue): AirmassRange = {
    val minimum = if (this.deciMin.value <= deciMax.value) this.deciMin else deciMax
    new AirmassRange(minimum, deciMax) {}
  }
}

object AirmassRange extends AirmassRangeOptics {
  type DeciValue    = Interval.Closed[10, 30]
  type IntDeciValue = Int Refined DeciValue

  /**
   * Construct a new AirmassRange.
   * Maintains the invariant that min <= max, swapping parameter order if needed.
   * @group Optics
   */
  def apply(deciMin: IntDeciValue, deciMax: IntDeciValue): AirmassRange =
    fromDeciVals.get((deciMin, deciMax))

  implicit val airmassRangeEq: Eq[AirmassRange] = Eq.fromUniversalEquals
}

trait AirmassRangeOptics {
  import AirmassRange.IntDeciValue

  /** @group Optics */
  lazy val fromDeciVals: SplitEpi[(IntDeciValue, IntDeciValue), AirmassRange] =
    SplitEpi(
      t => {
        val (min, max) = t
        if (min.value <= max.value) (new AirmassRange(min, max) {})
        else (new AirmassRange(max, min)                        {})
      },
      a => (a.deciMin, a.deciMax)
    )
}

sealed abstract case class HourAngleRange protected (
  deciMin: HourAngleRange.IntDeciHour,
  deciMax: HourAngleRange.IntDeciHour
) extends ElevationRange {
  def minDoubleHours: Double = deciMin.value / 10.0
  def maxDoubleHours: Double = deciMax.value / 10.0

  def minHourAngle: HourAngle = HourAngle.fromDoubleHours(minDoubleHours)
  def maxHourAngle: HourAngle = HourAngle.fromDoubleDegrees(maxDoubleHours)

  def minAngle: Angle = HourAngle.angle.get(minHourAngle)
  def maxAngle: Angle = HourAngle.angle.get(maxHourAngle)

  // Declination is used by Contraint.ElevationConstraint
  def minDeclination: Declination = Declination.fromAngleWithCarry(minAngle)._1
  def maxDeclination: Declination = Declination.fromAngleWithCarry(maxAngle)._1

  /**
   * Set the min value, and perhaps the max value.
   * Maintains the invariant the min <= max by modifying max to be min if necessary.
   */
  def setMin(deciMin: HourAngleRange.IntDeciHour): HourAngleRange = {
    val maximum = if (deciMin.value <= this.deciMax.value) this.deciMax else deciMin
    new HourAngleRange(deciMin, maximum) {}
  }

  /**
   * Set the max value, and perhaps the min value.
   * Maintains the invariant the min <= max by modifying min to be max if necessary.
   */
  def setMax(deciMax: HourAngleRange.IntDeciHour): HourAngleRange = {
    val minimum = if (this.deciMin.value <= deciMax.value) this.deciMin else deciMax
    new HourAngleRange(minimum, deciMax) {}
  }
}

object HourAngleRange extends HourAngleRangeOptics {
  type DeciHour    = Interval.Closed[-50, 50]
  type IntDeciHour = Int Refined DeciHour

  def apply(deciMin: IntDeciHour, deciMax: IntDeciHour): HourAngleRange =
    fromDeciHours.get((deciMin, deciMax))

  implicit val hourAngleRangeEq: Eq[HourAngleRange] = Eq.fromUniversalEquals
}

trait HourAngleRangeOptics {
  import HourAngleRange.IntDeciHour

  lazy val fromDeciHours: SplitEpi[(IntDeciHour, IntDeciHour), HourAngleRange] =
    SplitEpi(
      t => {
        val (min, max) = t
        if (min.value <= max.value) (new HourAngleRange(min, max) {})
        else (new HourAngleRange(max, min)                        {})
      },
      a => (a.deciMin, a.deciMax)
    )
}
