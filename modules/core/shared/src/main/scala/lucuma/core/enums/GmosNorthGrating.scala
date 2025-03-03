// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.units.{*, given}
import lucuma.core.util.Enumerated
import spire.math.Rational

/**
 * Enumerated type for GMOS North gratings.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
sealed abstract class GmosNorthGrating(
  val tag:                  String,
  val shortName:            String,
  val longName:             String,
  val rulingDensity:        Int,
  val dispersion:           Quantity[Rational, NanometersPerPixel],
  val simultaneousCoverage: WavelengthDelta,
  val blazeWavelength:      Wavelength,
  val referenceResolution:  PosInt
) extends Product with Serializable {

  /**
   * Δλ for 0.5" slit.
   * @see http://hyperphysics.phy-astr.gsu.edu/hbase/phyopt/gratres.html
   */
  private def Δλ: Double =
    blazeWavelength.nm.value.value.doubleValue / referenceResolution.value.toDouble

  /** Resolution at λ with the specified slit width. */
  def resolution(λ: Wavelength, slitWidth: Angle): Int =
    ((λ.nm.value.value.doubleValue / Δλ) * (0.5 / Angle.signedDecimalArcseconds.get(slitWidth).toDouble)).toInt

  /** Resolution at λ with the effective slit width of the given FPU. */
  def resolution(λ: Wavelength, fpu: GmosNorthFpu): Int =
    resolution(λ, fpu.effectiveSlitWidth)

}

object GmosNorthGrating {

  private def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
    PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].toValue[Rational].toUnit[NanometersPerPixel]

  private def nmToWavelengthDelta(value: Int): WavelengthDelta =
    WavelengthDelta.fromIntNanometers(value).get

  private def blazeNm(value: Int): Wavelength =
    Wavelength.fromIntNanometers(value).get

  private def resolution(value: Int): PosInt =
    PosInt.unsafeFrom(value)

  /** @group Constructors */
  case object B1200_G5301 extends GmosNorthGrating(
    tag                  = "B1200_G5301",
    shortName            = "B1200",
    longName             = "B1200_G5301",
    rulingDensity        = 1200,
    dispersion           = pmToDispersion( 26),
    simultaneousCoverage = nmToWavelengthDelta( 164),
    blazeWavelength      = blazeNm( 463),
    referenceResolution  = resolution(3744)
  )

  /** @group Constructors */
  case object R831_G5302  extends GmosNorthGrating(
    tag                  = "R831_G5302",
    shortName            = "R831",
    longName             = "R831_G5302",
    rulingDensity        = 831,
    dispersion           = pmToDispersion( 38),
    simultaneousCoverage = nmToWavelengthDelta( 235),
    blazeWavelength      = blazeNm(757),
    referenceResolution  = resolution(4396)
  )

  /** @group Constructors */
  case object B600_G5307  extends GmosNorthGrating(
    tag                  = "B600_G5307",
    shortName            = "B600",
    longName             = "B600_G5307",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 50),
    simultaneousCoverage = nmToWavelengthDelta( 317),
    blazeWavelength      = blazeNm( 461),
    referenceResolution  = resolution(1688)
  )

  /** @group Constructors */
  case object R600_G5304  extends GmosNorthGrating(
    tag                  = "R600_G5304",
    shortName            = "R600",
    longName             = "R600_G5304",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 52),
    simultaneousCoverage = nmToWavelengthDelta( 328),
    blazeWavelength      = blazeNm( 926),
    referenceResolution  = resolution(3744)
  )

  /** @group Constructors */
  case object B480_G5309  extends GmosNorthGrating(
    tag                  = "B480_G5309",
    shortName            = "B480",
    longName             = "B480_G5309",
    rulingDensity        = 480,
    dispersion           = pmToDispersion( 62),
    simultaneousCoverage = nmToWavelengthDelta( 390),
    blazeWavelength      = blazeNm( 422),
    referenceResolution  = resolution(1520)
  )

  /** @group Constructors */
  case object R400_G5305  extends GmosNorthGrating(
    tag                  = "R400_G5305",
    shortName            = "R400",
    longName             = "R400_G5305",
    rulingDensity        = 400,
    dispersion           = pmToDispersion( 74),
    simultaneousCoverage = nmToWavelengthDelta( 472),
    blazeWavelength      = blazeNm( 764),
    referenceResolution  = resolution(1918)
  )

  /** @group Constructors */
  case object R150_G5308  extends GmosNorthGrating(
    tag                  = "R150_G5308",
    shortName            = "R150",
    longName             = "R150_G5308",
    rulingDensity        = 150,
    dispersion           = pmToDispersion(193),
    simultaneousCoverage = nmToWavelengthDelta(1219),
    blazeWavelength      = blazeNm( 717),
    referenceResolution  = resolution(631)
  )

  /** All members of GmosNorthDisperser, in canonical order. */
  lazy val all: List[GmosNorthGrating] =
    List(B1200_G5301, R831_G5302, B600_G5307, R600_G5304, B480_G5309, R400_G5305, R150_G5308)

  /** Select the member of GmosNorthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthGrating] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosNorthGrating =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosNorthDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosNorthGratingEnumerated: Enumerated[GmosNorthGrating] =
    new Enumerated[GmosNorthGrating] {
      def all: List[GmosNorthGrating] = GmosNorthGrating.all
      def tag(a: GmosNorthGrating): String = a.tag
      override def unsafeFromTag(s: String): GmosNorthGrating =
        GmosNorthGrating.unsafeFromTag(s)
    }

}
