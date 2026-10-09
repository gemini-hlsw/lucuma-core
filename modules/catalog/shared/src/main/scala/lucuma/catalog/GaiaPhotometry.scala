// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue

import scala.collection.immutable.SortedMap

/**
 * Transformations from Gaia photometry to other systems, from the Gaia DR3 documentation, section
 * 5.5.1 "Photometric relationships with other photometric systems":
 * https://gea.esac.esa.int/archive/documentation/GDR3/Data_processing/chap_cu5pho/cu5pho_sec_photSystem/cu5pho_ssec_photRelations.html
 *
 * Every relation has the form G - X = c0 + c1 x + c2 x^2 + ..., with x = BP - RP, and is only valid
 * inside a colour range. The polynomials run to 4th order and stop behaving physically beyond their
 * range, so no estimate is produced outside it.
 */
object GaiaPhotometry:

  /** Polynomial in BP - RP giving G - X for some band X, valid inside a colour range. */
  final case class Transformation(
    band:         Band,
    coefficients: List[Double],
    minBpMinusRp: Double,
    maxBpMinusRp: Double
  ):
    def gMinusX(bpMinusRp: Double): Double =
      coefficients.zipWithIndex.foldLeft(0.0): (acc, ci) =>
        val (c, i) = ci
        acc + c * math.pow(bpMinusRp, i.toDouble)

    def inRange(bpMinusRp: Double): Boolean =
      bpMinusRp >= minBpMinusRp && bpMinusRp <= maxBpMinusRp

    /**
     * X estimated from G and the colour, if the colour is in the validity range. Rounded to
     * millimagnitudes, well below the scatter of any of the fits.
     */
    def estimate(g: BrightnessValue, bpMinusRp: Double): Option[BrightnessValue] =
      Option
        .when(inRange(bpMinusRp))(gMinusX(bpMinusRp))
        .flatMap: d =>
          val x = (g.value.value - BigDecimal(d)).setScale(3, BigDecimal.RoundingMode.HALF_UP)
          BrightnessValue.from(x).toOption

  // Johnson-Cousins V, SDSS g r i and 2MASS J H Ks. Ks is stored as K.
  val V: Transformation      =
    Transformation(Band.V, List(-0.02704, 0.01424, -0.2156, 0.01426), -0.5, 5.0)
  val SloanG: Transformation =
    Transformation(Band.SloanG, List(0.2199, -0.6365, -0.1548, 0.0064), 0.3, 3.0)
  val SloanR: Transformation =
    Transformation(Band.SloanR, List(-0.09837, 0.08592, 0.1907, -0.1701, 0.02263), 0.0, 3.0)
  val SloanI: Transformation =
    Transformation(Band.SloanI, List(-0.293, 0.6404, -0.09609, -0.002104), 0.5, 2.0)
  val J: Transformation      =
    Transformation(Band.J, List(0.01798, 1.389, -0.09338), -0.5, 2.5)
  val H: Transformation      =
    Transformation(Band.H, List(-0.1048, 2.011, -0.1758), -0.5, 2.5)
  val K: Transformation      =
    Transformation(Band.K, List(-0.0981, 2.089, -0.1579), -0.5, 2.5)

  /** Transformations attached to blind offset candidates, keyed by band. */
  val Transformations: SortedMap[Band, Transformation] =
    SortedMap.from(List(V, SloanG, SloanR, SloanI, J, H, K).map(t => t.band -> t))

  /**
   * Johnson-Cousins R, used for Altair guide star limits. Table 5.7 of the same section, valid for
   * 0.0 < BP-RP < 4.0 with a scatter of 0.032 mag. It is excluded from `Transformations`.
   */
  private val GMinusRCoefficients: List[Double] =
    List(-0.02275, 0.3961, -0.1243, -0.01396, 0.003775)

  val MinBpMinusRp: Double = 0.0
  val MaxBpMinusRp: Double = 4.0

  /** Difference G - R for a given BP - RP colour, without checking the validity range. */
  def gMinusR(bpMinusRp: Double): Double =
    GMinusRCoefficients.zipWithIndex.foldLeft(0.0): (acc, ci) =>
      val (c, i) = ci
      acc + c * math.pow(bpMinusRp, i.toDouble)

  /**
   * Conservative bounds on G - R over the validity range, padded outward to 0.01 mag. A query on G
   * widened by them never drops a star whose R falls in the requested range.
   */
  val GMinusRBounds: (Double, Double) =
    val steps: Int            = 4000
    val samples: List[Double] =
      (0 to steps).toList.map(i =>
        gMinusR(MinBpMinusRp + i * (MaxBpMinusRp - MinBpMinusRp) / steps)
      )
    (math.floor(samples.min * 100) / 100, math.ceil(samples.max * 100) / 100)

  /** Johnson-Cousins R estimated from Gaia G, BP and RP, if the colour is in the validity range. */
  def johnsonCousinsR(
    g:  BrightnessValue,
    bp: BrightnessValue,
    rp: BrightnessValue
  ): Option[BrightnessValue] =
    val colour: Double = (bp.value.value - rp.value.value).toDouble
    Option
      .when(colour > MinBpMinusRp && colour < MaxBpMinusRp)(colour)
      .flatMap(c => BrightnessValue.from(g.value.value - BigDecimal(gMinusR(c))).toOption)

  /**
   * R estimated from the G, BP and RP entries of a set of brightnesses, when all three are present
   * and the colour is in the validity range.
   */
  def estimatedR(brightnesses: SortedMap[Band, BrightnessValue]): Option[BrightnessValue] =
    (brightnesses.get(Band.Gaia), brightnesses.get(Band.GaiaBP), brightnesses.get(Band.GaiaRP))
      .flatMapN(johnsonCousinsR)

  /**
   * Every magnitude in `Transformations` that can be estimated from the G, BP and RP entries of a
   * set of brightnesses. Empty when G, BP or RP is missing.
   */
  def estimatedBrightnesses(
    brightnesses: SortedMap[Band, BrightnessValue]
  ): SortedMap[Band, BrightnessValue] =
    (brightnesses.get(Band.Gaia), brightnesses.get(Band.GaiaBP), brightnesses.get(Band.GaiaRP))
      .mapN: (g, bp, rp) =>
        val colour = (bp.value.value - rp.value.value).toDouble
        Transformations.flatMap((band, t) => t.estimate(g, colour).tupleLeft(band))
      .getOrElse(SortedMap.empty)
