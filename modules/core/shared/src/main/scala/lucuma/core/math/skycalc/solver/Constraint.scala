// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc.solver

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.skycalc.SkyCalcResults

import java.time.Instant

/**
  * Defines whether a condition is met at an [[java.time.Instant]] among [[Samples]].
  *
  * @tparam T the type of results held by the [[Samples]]
  * @tparam A the type on which the condition is checked (usually obtainable from <code>T</code>)
  */
trait Constraint[T, A] {

  /**
    * Defines the actual constraint by returning true or false for a given instant <code>i</code>
    * depending on whether to constraint is met or not.
    */
  def metAt[R: SampleRounder[*, A]](samples: Samples[T])(i: Instant): Boolean
}

case class ElevationConstraint(min: Declination, max: Declination)
    extends Constraint[SkyCalcResults, Declination] {
  override def metAt[R: SampleRounder[*, Declination]](
    samples: Samples[SkyCalcResults]
  )(i:       Instant): Boolean =
    samples
      .map(_.altitude)
      .valueAt(i)
      .map { elevation =>
        elevation >= min && elevation <= max
      }
      .getOrElse(false)
}

case class SkyBrightnessConstraint(min: Double, max: Double)
    extends Constraint[SkyCalcResults, Double] {
  override def metAt[R: SampleRounder[*, Double]](
    samples: Samples[SkyCalcResults]
  )(i:       Instant): Boolean =
    samples
      .map(_.totalSkyBrightness)
      .valueAt(i)
      .map { skyBrightness =>
        skyBrightness >= min && skyBrightness <= max
      }
      .getOrElse(false)
}

case class AirMassConstraint(min: Double, max: Double)
    extends Constraint[SkyCalcResults, (Double, Declination)] {

  private val MinElevation: Declination =
    Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(5.0))._1

  override def metAt[R: SampleRounder[*, (Double, Declination)]](
    samples: Samples[SkyCalcResults]
  )(i:       Instant): Boolean =
    samples
      .map(r => (r.airmass, r.altitude))
      .valueAt(i)
      .map {
        case (airmass, elevation) =>
          // NOTE: we need to work around errors with interpolation etc which may cause to give wrong airmass values for very small altitudes (<1deg)
          elevation >= MinElevation && airmass >= min && airmass <= max
      }
      .getOrElse(false)

}

case class HourAngleConstraint(min: HourAngle, max: HourAngle)
    extends Constraint[SkyCalcResults, (HourAngle, Declination)] {

  private val MinElevation: Declination =
    Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(5.0))._1

  override def metAt[R: SampleRounder[*, (HourAngle, Declination)]](
    samples: Samples[SkyCalcResults]
  )(i:       Instant): Boolean =
    samples
      .map(r => (r.hourAngle, r.altitude))
      .valueAt(i)
      .map {
        case (hourAngle, elevation) =>
          // NOTE: we need to work around errors with interpolation etc which may cause to give wrong hour angle values for very small altitudes (<1deg)
          elevation >= MinElevation && hourAngle.toMicroarcseconds >= min.toMicroarcseconds && hourAngle.toMicroarcseconds <= max.toMicroarcseconds
      }
      .getOrElse(false)

}

// MoonCalculator is not ported yet.
// case class MoonElevationConstraint(
//   min:       Double,
//   max:       Double,
//   tolerance: Long = Duration.ofSeconds(30).toMillis
// ) extends Constraint[MoonCalculator] {
//   protected val solver = DefaultSolver[MoonCalculator](tolerance)
//   def metAt(t: Long, moon: MoonCalculator): Boolean = {
//     val elevation = moon.elevationAt(t)
//     elevation >= min && elevation <= max
//   }
// }
