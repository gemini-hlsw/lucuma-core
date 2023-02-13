// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import eu.timepit.refined.numeric.*
import lucuma.core.util.RefinedNewType

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
object BrightnessValue extends RefinedNewType[BigDecimal, BrightnessValueRefinement]
type BrightnessValue = BrightnessValue.Type

// The line width must be positive. For upper limit, we could probably safely use 1e6 km/s.
type LineWidthValueRefinement = Interval.OpenClosed[0, 1_000_000]
object LineWidthValue extends RefinedNewType[BigDecimal, LineWidthValueRefinement]
type LineWidthValue = LineWidthValue.Type

// Should never be less than zero or larger than 1.
type LineFluxValueRefinement = Interval.Closed[0, 1]
object LineFluxValue extends RefinedNewType[BigDecimal, LineFluxValueRefinement]
type LineFluxValue = LineFluxValue.Type

// Should never be less than zero or larger than 1.
type FluxDensityContinuumValueRefinement = Interval.Closed[0, 1]
object FluxDensityContinuumValue extends RefinedNewType[BigDecimal, FluxDensityContinuumValueRefinement]
type FluxDensityContinuumValue = FluxDensityContinuumValue.Type
