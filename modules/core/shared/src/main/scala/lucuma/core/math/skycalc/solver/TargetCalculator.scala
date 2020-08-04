// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import gsp.math.Coordinates
import gsp.math.Place
import gsp.math.skycalc.ImprovedSkyCalc

import java.time.Duration
import java.time.Instant
import gsp.math.skycalc.SkyCalcResults

/**
  * Target calculator that allows to calculate different attributes of a target for a given set of instants.
  *
  * If in doubt use {@link isDefinedAt} to make sure that values for a given time are actually calculated before
  * accessing them, otherwise an out of bounds exception will be thrown.
  */
trait TargetCalculator[G] extends Calculator[G, SkyCalcResults] {

  val place: Place
  val targetLocation: Instant => Coordinates

  val skycalc = ImprovedSkyCalc(place)

  val result: Instant => SkyCalcResults = i => skycalc.calculate(targetLocation(i), i, true)

  // If the target is visible during the scheduled time, return the weighted mean parallactic angle as Some(angle in degrees).
  // Otherwise, the target is not visible, so return None.
  lazy val weightedMeanParallacticAngle: Option[Double] = {
    val (weightedAngles, weights) = results
      .map(_.parallacticAngleRaw)
      .zip(instants)
      .zip(results.map(_.airmass))
      .map {
        case ((angle, t), airmass) =>
          // Wrap negative angles as per Andy's comment in OCSADV-16.
          val normalizedAngle = {
            if (angle < 0) {
              val normalizingFactor = {
                val dec = targetLocation(t).dec.toAngle.toSignedDoubleDegrees
                if (dec - place.latitude.toAngle.toSignedDoubleDegrees < -10) 0
                else if (dec - place.latitude.toAngle.toSignedDoubleDegrees < 10) 180
                else 360
              }
              angle + normalizingFactor
            } else angle
          }

          //val weight = if (airmass <= 1.0) 0.0 else 1.6 * math.pow(airmass - 1.0, 0.6)
          val weight = if (airmass <= 1.0) 0.0 else math.pow(airmass - 1.0, 1.3)
          (normalizedAngle * weight, weight)
      }
      .unzip

    val weightedSum = weights.sum
    if (weightedSum == 0)
      None
    else
      Some(weightedAngles.sum / weightedSum)
  }
}

case class SingleValueTargetCalculator(
  place:          Place,
  targetLocation: Instant => Coordinates,
  instant:        Instant
) extends SingleValueCalculator[SkyCalcResults]
    with TargetCalculator[GetterStrategy.Exact]

case class IntervalTargetCalculator(
  place:          Place,
  targetLocation: Instant => Coordinates,
  interval:       Interval,
  rate:           Duration
) extends FixedRateCalculator[SkyCalcResults]
    with TargetCalculator[GetterStrategy.LinearInterpolating]

case class SampleTargetCalculator(
  place:          Place,
  targetLocation: Instant => Coordinates,
  instants:       List[Instant]
) extends IrregularIntervalCalculator[SkyCalcResults]
    with TargetCalculator[GetterStrategy.LinearInterpolating]

object TargetCalculator {
  def apply(
    place:          Place,
    targetLocation: Instant => Coordinates,
    instant:        Instant
  ): SingleValueTargetCalculator =
    SingleValueTargetCalculator(place, targetLocation, instant)

  def apply(
    place:          Place,
    targetLocation: Instant => Coordinates,
    interval:       Interval,
    rate:           Duration = Duration.ofSeconds(30)
  ): IntervalTargetCalculator =
    IntervalTargetCalculator(place, targetLocation, interval, rate)

  def apply(
    place:          Place,
    targetLocation: Instant => Coordinates,
    instants:       List[Instant]
  ): SampleTargetCalculator =
    SampleTargetCalculator(place, targetLocation, instants)

}
