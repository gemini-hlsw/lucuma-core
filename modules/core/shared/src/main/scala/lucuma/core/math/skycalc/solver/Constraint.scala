// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Duration
import java.time.Instant
import gsp.math.Angle
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.skycalc.SkyCalcResults

trait Constraint[T, A] {
  protected val solver: Solver[A]

  def solve[G](calc: Calculator[G, T])(interval: Interval)(implicit
    getter:          CalcGetter[G, A]
  ): Schedule =
    solver.solve(metAt(calc))(interval)

  /**
    * This function defines the actual constraint by returning true or false for a given instant <code>i</code>
    * depending on whether to constraint is met or not.
    */
  def metAt[G](calc: Calculator[G, T])(i: Instant)(implicit getter: CalcGetter[G, A]): Boolean
}

/**
  * Implementation for an elevation constraint that uses the pre-calculated data from a
  * {@see edu.gemini.util.skycalc.calc.TargetCalc} object.
  */
case class ElevationConstraint(
  min:       Declination,
  max:       Declination,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, Declination] {
  protected val solver = DefaultSolver[Declination](tolerance)
  override def metAt[G](
    calc: Calculator[G, SkyCalcResults]
  )(i:    Instant)(implicit getter: CalcGetter[G, Declination]): Boolean = {
    val elevation = calc.valueAt(_.altitude)(i)
    elevation >= min && elevation <= max
  }
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

case class SkyBrightnessConstraint(
  min:       Double,
  max:       Double,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, Double] {
  protected val solver = DefaultSolver[Double](tolerance)
  override def metAt[G](
    calc: Calculator[G, SkyCalcResults]
  )(i:    Instant)(implicit getter: CalcGetter[G, Double]): Boolean = {
    val skyBrightness = calc.valueAt(_.totalSkyBrightness)(i)
    skyBrightness >= min && skyBrightness <= max
  }
}

case class AirmassConstraint(
  min:       Double,
  max:       Double,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, (Double, Declination)] {
  private val MinElevation: Declination =
    Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(5.0))._1
  protected val solver                  = DefaultSolver[(Double, Declination)](tolerance)
  override def metAt[G](
    calc: Calculator[G, SkyCalcResults]
  )(i:    Instant)(implicit getter: CalcGetter[G, (Double, Declination)]): Boolean = {
    val (airmass, elevation) = calc.valueAt(r => (r.airmass, r.altitude))(i)
    // NOTE: we need to work around errors with interpolation etc which may cause to give wrong airmass values for very small altitudes (<1deg)
    elevation >= MinElevation && airmass >= min && airmass <= max
  }
}

case class HourAngleConstraint(
  min:       HourAngle,
  max:       HourAngle,
  tolerance: Duration = Duration.ofSeconds(30)
) extends Constraint[SkyCalcResults, (HourAngle, Declination)] {
  private val MinElevation: Declination =
    Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(5.0))._1
  protected val solver                  = DefaultSolver[(HourAngle, Declination)](tolerance)
  override def metAt[G](
    calc: Calculator[G, SkyCalcResults]
  )(i:    Instant)(implicit getter: CalcGetter[G, (HourAngle, Declination)]): Boolean = {
    val (hourAngle, elevation) = calc.valueAt(r => (r.hourAngle, r.altitude))(i)
    // NOTE: we need to work around errors with interpolation etc which may cause to give wrong hour angle values for very small altitudes (<1deg)
    elevation >= MinElevation && hourAngle.toMicroarcseconds >= min.toMicroarcseconds && hourAngle.toMicroarcseconds <= max.toMicroarcseconds
  }
}
