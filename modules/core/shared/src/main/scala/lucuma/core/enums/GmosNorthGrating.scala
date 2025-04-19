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
 * Enumerated type for GMOS North gratings.
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
enum GmosNorthGrating(
  val tag:                  String,
  val shortName:            String,
  val longName:             String,
  val rulingDensity:        Int,
  val dispersion:           Quantity[Rational, NanometersPerPixel],
  val simultaneousCoverage: WavelengthDelta,
  override val blazeWavelength:      Wavelength,
  override val referenceResolution:  PosInt
) extends GmosGrating derives Enumerated:

  case B1200_G5301 extends GmosNorthGrating(
    tag                  = "B1200_G5301",
    shortName            = "B1200",
    longName             = "B1200_G5301",
    rulingDensity        = 1200,
    dispersion           = pmToDispersion( 26),
    simultaneousCoverage = nmToWavelengthDelta( 164),
    blazeWavelength      = blazeNm( 463),
    referenceResolution  = resolution(3744)
  )

  case R831_G5302  extends GmosNorthGrating(
    tag                  = "R831_G5302",
    shortName            = "R831",
    longName             = "R831_G5302",
    rulingDensity        = 831,
    dispersion           = pmToDispersion( 38),
    simultaneousCoverage = nmToWavelengthDelta( 235),
    blazeWavelength      = blazeNm(757),
    referenceResolution  = resolution(4396)
  )

  case R600_G5304  extends GmosNorthGrating(
    tag                  = "R600_G5304",
    shortName            = "R600",
    longName             = "R600_G5304",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 52),
    simultaneousCoverage = nmToWavelengthDelta( 328),
    blazeWavelength      = blazeNm( 926),
    referenceResolution  = resolution(3744)
  )

  case B480_G5309  extends GmosNorthGrating(
    tag                  = "B480_G5309",
    shortName            = "B480",
    longName             = "B480_G5309",
    rulingDensity        = 480,
    dispersion           = pmToDispersion( 62),
    simultaneousCoverage = nmToWavelengthDelta( 390),
    blazeWavelength      = blazeNm( 422),
    referenceResolution  = resolution(1520)
  )

  case R400_G5305  extends GmosNorthGrating(
    tag                  = "R400_G5305",
    shortName            = "R400",
    longName             = "R400_G5305",
    rulingDensity        = 400,
    dispersion           = pmToDispersion( 74),
    simultaneousCoverage = nmToWavelengthDelta( 472),
    blazeWavelength      = blazeNm( 764),
    referenceResolution  = resolution(1918)
  )

  case R150_G5308  extends GmosNorthGrating(
    tag                  = "R150_G5308",
    shortName            = "R150",
    longName             = "R150_G5308",
    rulingDensity        = 150,
    dispersion           = pmToDispersion(193),
    simultaneousCoverage = nmToWavelengthDelta(1219),
    blazeWavelength      = blazeNm( 717),
    referenceResolution  = resolution(631)
  )
