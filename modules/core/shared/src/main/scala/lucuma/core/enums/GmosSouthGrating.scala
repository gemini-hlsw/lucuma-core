// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq._
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Coverage
import lucuma.core.math.Wavelength
import lucuma.core.math.units.{_, given}
import lucuma.core.util.Enumerated
import spire.math.Rational

/**
 * Enumerated type for GMOS South gratings.
 * @group Enumerations (Generated)
 * @see https://www.gemini.edu/instrumentation/gmos/components#Gratings
 */
sealed abstract class GmosSouthGrating(
  val tag:                  String,
  val shortName:            String,
  val longName:             String,
  val rulingDensity:        Int,
  val dispersion:           Quantity[Rational, NanometersPerPixel],
  val simultaneousCoverage: Quantity[PosInt, Nanometer],
  val blazeWavelength:      Wavelength,
  val referenceResolution:  PosInt,
  val obsolete:             Boolean
) extends Product with Serializable {

  /** Compute the coverage of this disperser, given a central wavelength. */
  def coverage(λ: Wavelength): Coverage =
    Coverage.centered(λ, simultaneousCoverage.tToUnit[Picometer])

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
  def resolution(λ: Wavelength, fpu: GmosSouthFpu): Int =
    resolution(λ, fpu.effectiveSlitWidth)

}

object GmosSouthGrating {

  private def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
    PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].toValue[Rational].toUnit[NanometersPerPixel]

  private def nm(value: Int): Quantity[PosInt, Nanometer] =
    PosInt.unsafeFrom(value).withUnit[Nanometer]

  private def blazeNm(value: Int): Wavelength =
    Wavelength.fromIntNanometers(value).get

  private def resolution(value: Int): PosInt =
    PosInt.unsafeFrom(value)

  /** @group Constructors */
  case object B1200_G5321 extends GmosSouthGrating(
    tag                  = "B1200_G5321",
    shortName            = "B1200",
    longName             = "B1200_G5321",
    rulingDensity        = 1200,
    dispersion           = pmToDispersion( 26),
    simultaneousCoverage = nm( 159),
    blazeWavelength      = blazeNm( 463),
    referenceResolution  = resolution(3744),
    obsolete             = false
  )

  /** @group Constructors */
  case object R831_G5322  extends GmosSouthGrating(
    tag                  = "R831_G5322",
    shortName            = "R831",
    longName             = "R831_G5322",
    rulingDensity        = 831,
    dispersion           = pmToDispersion( 38),
    simultaneousCoverage = nm( 230),
    blazeWavelength      = blazeNm(757),
    referenceResolution  = resolution(4396),
    obsolete             = false
  )

  /** @group Constructors */
  case object B600_G5323  extends GmosSouthGrating(
    tag                  = "B600_G5323",
    shortName            = "B600",
    longName             = "B600_G5323",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 50),
    simultaneousCoverage = nm( 307),
    blazeWavelength      = blazeNm( 461),
    referenceResolution  = resolution(1688),
    obsolete             = false
  )

  /** @group Constructors */
  case object R600_G5324  extends GmosSouthGrating(
    tag                  = "R600_G5324",
    shortName            = "R600",
    longName             = "R600_G5324",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 52),
    simultaneousCoverage = nm( 318),
    blazeWavelength      = blazeNm( 926),
    referenceResolution  = resolution(3744),
    obsolete             = false
  )

  /** @group Constructors */
  case object B480_G5327  extends GmosSouthGrating(
    tag                  = "B480_G5327",
    shortName            = "B480",
    longName             = "B480_G5327",
    rulingDensity        = 480,
    dispersion           = pmToDispersion( 62),
    simultaneousCoverage = nm( 390),
    blazeWavelength      = blazeNm( 422),
    referenceResolution  = resolution(1520),
    obsolete             = false
  )

  /** @group Constructors */
  case object R400_G5325  extends GmosSouthGrating(
    tag                  = "R400_G5325",
    shortName            = "R400",
    longName             = "R400_G5325",
    rulingDensity        = 400,
    dispersion           = pmToDispersion( 74),
    simultaneousCoverage = nm( 462),
    blazeWavelength      = blazeNm( 764),
    referenceResolution  = resolution(1918),
    obsolete             = false
  )

  /** @group Constructors */
  case object R150_G5326  extends GmosSouthGrating(
    tag                  = "R150_G5326",
    shortName            = "R150",
    longName             = "R150_G5326",
    rulingDensity        = 150,
    dispersion           = pmToDispersion(193),
    simultaneousCoverage = nm(1190),
    blazeWavelength      = blazeNm( 717),
    referenceResolution  = resolution(631),
    obsolete             = false
  )

  /** All members of GmosSouthDisperser, in canonical order. */
  lazy val all: List[GmosSouthGrating] =
    List(B1200_G5321, R831_G5322, B600_G5323, R600_G5324, B480_G5327, R400_G5325, R150_G5326)

  /** Select the member of GmosSouthDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthGrating] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthGrating =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthGratingEnumerated: Enumerated[GmosSouthGrating] =
    new Enumerated[GmosSouthGrating] {
      def all: List[GmosSouthGrating] = GmosSouthGrating.all
      def tag(a: GmosSouthGrating): String = a.tag
      override def unsafeFromTag(s: String): GmosSouthGrating =
        GmosSouthGrating.unsafeFromTag(s)
    }

}
