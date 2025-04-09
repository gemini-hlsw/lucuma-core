// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import eu.timepit.refined.numeric.*
import lucuma.core.util.NewRefined

/** Mathematical data types for general use, not specific to the Gem model. */
type RA = RightAscension
val RA: RightAscension.type = RightAscension

type Dec = Declination
val Dec: Declination.type = Declination

type Lat = Declination
val Lat: Declination.type = Declination

type Lon = Angle
val Lon: Angle.type = Angle

// The sun is -27 magnitudes. Maximum in Janskys is 1e8.
type BrightnessValueRefinement = Interval.Closed[-30, 100_000_000]
object BrightnessValue extends NewRefined[BigDecimal, BrightnessValueRefinement]
type BrightnessValue = BrightnessValue.Type

// The line width must be positive. For upper limit, we could probably safely use 1e6 km/s.
type LineWidthValueRefinement = Interval.OpenClosed[0, 1_000_000]
object LineWidthValue extends NewRefined[BigDecimal, LineWidthValueRefinement]
type LineWidthValue = LineWidthValue.Type

// Should never be less than zero or larger than 1.
type LineFluxValueRefinement = Interval.Closed[0, 1]
object LineFluxValue extends NewRefined[BigDecimal, LineFluxValueRefinement]
type LineFluxValue = LineFluxValue.Type

// Should never be less than zero or larger than 1.
type FluxDensityContinuumValueRefinement = Interval.Closed[0, 1]
object FluxDensityContinuumValue extends NewRefined[BigDecimal, FluxDensityContinuumValueRefinement]
type FluxDensityContinuumValue = FluxDensityContinuumValue.Type

// Copied from:
// https://github.com/ghewgill/picomath/blob/master/scala/src/Erf.scala
object Erf:
  import scala.math.{abs, exp}

  // constants
  private val a1: Double =  0.254829592
  private val a2: Double = -0.284496736
  private val a3: Double =  1.421413741
  private val a4: Double = -1.453152027
  private val a5: Double =  1.061405429
  private val p: Double  =  0.3275911

  // Calculates the error function using Horner’s method.
  def erf(x: Double): Double =
    // Save the sign of x
    val sign = if (x < 0) -1 else 1
    val absx =  abs(x)

    // A&S formula 7.1.26, rational approximation of error function
    val t = 1.0/(1.0 + p*absx);
    val y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
    sign*y

export Erf.erf
