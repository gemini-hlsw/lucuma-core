// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`

import cats.syntax.eq._
import coulomb._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Angle
import lucuma.core.math.Coverage
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
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
  val simultaneousCoverage: Quantity[PosInt, Nanometer],
  val blazeWavelength:      Wavelength,
  val referenceResolution:  PosInt,
  val obsolete:             Boolean
) extends Product with Serializable {

  /** Compute the coverage of this disperser, given a central wavelength. */
  def coverage(λ: Wavelength): Coverage =
    Coverage.centered(λ, simultaneousCoverage)

  /**
   * Δλ for 0.5" slit.
   * @see http://hyperphysics.phy-astr.gsu.edu/hbase/phyopt/gratres.html
   */
  private def Δλ: Double =
    blazeWavelength.nm.value.toDouble / referenceResolution.value.toDouble

  /** Resolution at λ with the specified slit width. */
  def resolution(λ: Wavelength, slitWidth: Angle): Int =
    ((λ.nm.value.toDouble / Δλ) * (0.5 / Angle.signedDecimalArcseconds.get(slitWidth).toDouble)).toInt

  /** Resolution at λ with the effective slit width of the given FPU. */
  def resolution(λ: Wavelength, fpu: GmosNorthFpu): Int =
    resolution(λ, fpu.effectiveSlitWidth)

}

object GmosNorthGrating {

  private def pmToDispersion(pm: Int): Quantity[Rational, NanometersPerPixel] =
    PosInt.unsafeFrom(pm).withUnit[PicometersPerPixel].to[Rational, NanometersPerPixel]

  private def nm(value: Int): Quantity[PosInt, Nanometer] =
    PosInt.unsafeFrom(value).withUnit[Nanometer]

  private def blazeNm(value: Int): Wavelength =
    Wavelength.fromNanometers(value).get

  private def resolution(value: Int): PosInt =
    PosInt.unsafeFrom(value)

  /** @group Constructors */
  case object B1200_G5301 extends GmosNorthGrating(
    tag                  = "B1200_G5301",
    shortName            = "B1200",
    longName             = "B1200_G5301",
    rulingDensity        = 1200,
    dispersion           = pmToDispersion( 26),
    simultaneousCoverage = nm( 164),
    blazeWavelength      = blazeNm( 463),
    referenceResolution  = resolution(3744),
    obsolete             = false
  )

  /** @group Constructors */
  case object R831_G5302  extends GmosNorthGrating(
    tag                  = "R831_G5302",
    shortName            = "R831",
    longName             = "R831_G5302",
    rulingDensity        = 831,
    dispersion           = pmToDispersion( 38),
    simultaneousCoverage = nm( 235),
    blazeWavelength      = blazeNm(757),
    referenceResolution  = resolution(4396),
    obsolete             = false
  )

  /** @group Constructors */
  case object B600_G5303  extends GmosNorthGrating(
    tag                  = "B600_G5303",
    shortName            = "B600",
    longName             = "B600_G5303",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 45),
    simultaneousCoverage = nm( 276),
    blazeWavelength      = blazeNm( 461),
    referenceResolution  = resolution(1688),
    obsolete             = true
  )

  /** @group Constructors */
  case object B600_G5307  extends GmosNorthGrating(
    tag                  = "B600_G5307",
    shortName            = "B600",
    longName             = "B600_G5307",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 50),
    simultaneousCoverage = nm( 317),
    blazeWavelength      = blazeNm( 461),
    referenceResolution  = resolution(1688),
    obsolete             = false
  )

  /** @group Constructors */
  case object R600_G5304  extends GmosNorthGrating(
    tag                  = "R600_G5304",
    shortName            = "R600",
    longName             = "R600_G5304",
    rulingDensity        = 600,
    dispersion           = pmToDispersion( 52),
    simultaneousCoverage = nm( 328),
    blazeWavelength      = blazeNm( 926),
    referenceResolution  = resolution(3744),
    obsolete             = false
  )

  /** @group Constructors */
  case object B480_G5309  extends GmosNorthGrating(
    tag                  = "B480_G5309",
    shortName            = "B480",
    longName             = "B480_G5309",
    rulingDensity        = 480,
    dispersion           = pmToDispersion( 62),
    simultaneousCoverage = nm( 390),
    blazeWavelength      = blazeNm( 422),
    referenceResolution  = resolution(1520),
    obsolete             = false
  )

  /** @group Constructors */
  case object R400_G5305  extends GmosNorthGrating(
    tag                  = "R400_G5305",
    shortName            = "R400",
    longName             = "R400_G5305",
    rulingDensity        = 400,
    dispersion           = pmToDispersion( 74),
    simultaneousCoverage = nm( 472),
    blazeWavelength      = blazeNm( 764),
    referenceResolution  = resolution(1918),
    obsolete             = false
  )

  /** @group Constructors */
  case object R150_G5306  extends GmosNorthGrating(
    tag                  = "R150_G5306",
    shortName            = "R150",
    longName             = "R150_G5306",
    rulingDensity        = 150,
    dispersion           = pmToDispersion(174),
    simultaneousCoverage = nm(1071),
    blazeWavelength      = blazeNm( 717),
    referenceResolution  = resolution(631),
    obsolete             = true
  )

  /** @group Constructors */
  case object R150_G5308  extends GmosNorthGrating(
    tag                  = "R150_G5308",
    shortName            = "R150",
    longName             = "R150_G5308",
    rulingDensity        = 150,
    dispersion           = pmToDispersion(193),
    simultaneousCoverage = nm(1219),
    blazeWavelength      = blazeNm( 717),
    referenceResolution  = resolution(631),
    obsolete             = false
  )

  /** All members of GmosNorthDisperser, in canonical order. */
  lazy val all: List[GmosNorthGrating] =
    List(B1200_G5301, R831_G5302, B600_G5303, B600_G5307, R600_G5304, B480_G5309, R400_G5305, R150_G5306, R150_G5308)

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
