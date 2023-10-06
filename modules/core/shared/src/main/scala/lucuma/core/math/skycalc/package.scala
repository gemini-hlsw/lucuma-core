// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.model.ObjectTracking

import java.time.Duration
import java.time.Instant
import scala.concurrent.duration.*

opaque type TimeRange = (Long, Long)
extension (r: TimeRange)
  def duration: Long = r._2 - r._1
  def start: Long    = r._1
  def end: Long      = r._2

// Calculate the parallactic angle for the given object at the given time.
def parallacticAngle(site: Site, tracking: ObjectTracking, vizTime: Instant): Angle = {
  val skycalc = new ImprovedSkyCalc(site.place)

  val c = tracking
    .at(vizTime)
    .map(_.value)
    .getOrElse(tracking.baseCoordinates)
  skycalc.calculate(c, vizTime, false).parallacticAngle
}

def averageParallacticAngle(site: Site, tracking: ObjectTracking, vizTime: Instant, duration: Duration): Option[Angle] =
  val defined: TimeRange = (vizTime.toEpochMilli(), vizTime.plus(duration).toEpochMilli())
  val rate: Long         = 30.seconds.toMillis

  // the number of samples we need to have a sampling rate >= than expected
  val cnt: Int            = math.ceil(defined.duration.toDouble / rate).toInt
  // the precise rate in milliseconds that corresponds to the expected rate
  val preciseRate: Double = defined.duration.toDouble / cnt

  /** Calculates a vector with times that cover the given interval. */
  val times: Vector[Long] = {
    val ts = for {
      i <- 0 to cnt
    } yield Math.ceil(defined.start + i * preciseRate).toLong // always round up
    require(ts.head == defined.start)
    require(ts.last >= defined.end)
    Vector(ts: _*)
  }

  def calculate(site: Site, tracking: ObjectTracking): Vector[SkyCalcResults] = {
    val skycalc = new ImprovedSkyCalc(site.place)

    times.traverse { t =>
      val at     = Instant.ofEpochMilli(t)
      val coords = tracking.at(at)
      coords.map(coords => skycalc.calculate(coords.value, Instant.ofEpochMilli(t), false))
    }.orEmpty
  }

  // If the target is visible during the scheduled time, return the weighted mean parallactic angle as Some(angle in degrees).
  // Otherwise, the target is not visible, so return None.
  def weightedMeanParallacticAngle(site: Site, tracking: ObjectTracking): Option[Double] = {
    val values                    = calculate(site, tracking)
    val (weightedAngles, weights) = values
      .map(_.parallacticAngle.toSignedDoubleDegrees)
      .zip(times)
      .zip(values.map(_.airmass))
      .map { case ((angle, t), airmass) =>
        // Wrap negative angles as per Andy's comment in OCSADV-16.
        val normalizedAngle =
          if (angle < 0) {
            val normalizingFactor = {
              val dec = tracking
                .at(Instant.ofEpochMilli(t))
                .map(_.value)
                .getOrElse(tracking.baseCoordinates)
                .dec
                .toAngle
                .toSignedDoubleDegrees
              if (dec - site.latitude.toAngle.toSignedDoubleDegrees < -10) 0
              else if (dec - site.latitude.toAngle.toSignedDoubleDegrees < 10) 180
              else 360
            }
            angle + normalizingFactor
          } else angle

        // val weight = if (airmass <= 1.0) 0.0 else 1.6 * math.pow(airmass - 1.0, 0.6)
        val weight = if (airmass <= 1.0) 0.0 else math.pow(airmass - 1.0, 1.3)
        (normalizedAngle * weight, weight)
      }
      .unzip

    val weightedSum = weights.sum
    if (weightedSum == 0) None
    else Some(weightedAngles.sum / weightedSum)
  }

  weightedMeanParallacticAngle(site, tracking).map(Angle.fromDoubleDegrees)

