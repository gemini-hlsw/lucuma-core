// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import coulomb.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.units.*
import lucuma.core.util.Enumerated
import spire.math.Rational

/**
 * Enumerated type for GMOS South gratings.
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
enum GmosSouthGrating(
  val tag:                  String,
  val shortName:            String,
  val longName:             String,
  val rulingDensity:        Int,
  val dispersion:           Quantity[Rational, NanometersPerPixel],
  val simultaneousCoverage: WavelengthDelta,
  override val blazeWavelength:      Wavelength,
  override val referenceResolution:  PosInt
) extends GmosGrating derives Enumerated:

  case B1200_G5321 extends GmosSouthGrating(
    tag                  = "B1200_G5321",
    shortName            = "B1200",
    longName             = "B1200_G5321",
    rulingDensity        = 1200,
    dispersion           = pmToDispersion( 26),
    simultaneousCoverage = nmToWavelengthDelta( 159),
    blazeWavelength      = blazeNm( 463),
    referenceResolution  = resolution(3744)
  )

  case R831_G5322  extends GmosSouthGrating(
    tag                  = "R831_G5322",
    shortName            = "R831",
    longName             = "R831_G5322",
    rulingDensity        = 831,
    dispersion           = pmToDispersion( 38),
    simultaneousCoverage = nmToWavelengthDelta( 230),
    blazeWavelength      = blazeNm(757),
    referenceResolution  = resolution(4396)
  )

  case R600_G5324  extends GmosSouthGrating(
    tag                  = "R600_G5324",
    shortName            = "R600",
    longName             = "R600_G5324",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 52),
    simultaneousCoverage = nmToWavelengthDelta( 318),
    blazeWavelength      = blazeNm( 926),
    referenceResolution  = resolution(3744)
  )

  case B480_G5327  extends GmosSouthGrating(
    tag                  = "B480_G5327",
    shortName            = "B480",
    longName             = "B480_G5327",
    rulingDensity        = 480,
    dispersion           = pmToDispersion( 62),
    simultaneousCoverage = nmToWavelengthDelta( 390),
    blazeWavelength      = blazeNm( 422),
    referenceResolution  = resolution(1520)
  )

  case R400_G5325  extends GmosSouthGrating(
    tag                  = "R400_G5325",
    shortName            = "R400",
    longName             = "R400_G5325",
    rulingDensity        = 400,
    dispersion           = pmToDispersion( 74),
    simultaneousCoverage = nmToWavelengthDelta( 462),
    blazeWavelength      = blazeNm( 764),
    referenceResolution  = resolution(1918)
  )

  case R150_G5326  extends GmosSouthGrating(
    tag                  = "R150_G5326",
    shortName            = "R150",
    longName             = "R150_G5326",
    rulingDensity        = 150,
    dispersion           = pmToDispersion(193),
    simultaneousCoverage = nmToWavelengthDelta(1190),
    blazeWavelength      = blazeNm( 717),
    referenceResolution  = resolution(631)
  )