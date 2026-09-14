// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue

import scala.collection.immutable.SortedMap

/**
 * Transformations from Gaia photometry to other systems. Gaia has no R band, but Altair guide star
 * limits are expressed in Johnson-Cousins R, so R is estimated from G and the BP-RP colour.
 *
 * Polynomial from the Gaia DR3 documentation, section 5.5.1 "Photometric relationships with other
 * photometric systems", Table 5.7, valid for 0.0 < BP-RP < 4.0 with a scatter of 0.032 mag:
 * https://gea.esac.esa.int/archive/documentation/GDR3/Data_processing/chap_cu5pho/cu5pho_sec_photSystem/cu5pho_ssec_photRelations.html
 *
 * G - R = -0.02275 + 0.3961 x - 0.1243 x^2 - 0.01396 x^3 + 0.003775 x^4, x = BP - RP
 */
object GaiaPhotometry:

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
   * Adds an estimated R to a set of brightnesses that has G, BP and RP but no R of its own. A
   * catalog R is never overwritten.
   */
  def withEstimatedR(
    brightnesses: SortedMap[Band, BrightnessValue]
  ): SortedMap[Band, BrightnessValue] =
    if brightnesses.contains(Band.R) then brightnesses
    else
      (brightnesses.get(Band.Gaia), brightnesses.get(Band.GaiaBP), brightnesses.get(Band.GaiaRP))
        .flatMapN(johnsonCousinsR)
        .fold(brightnesses)(r => brightnesses + (Band.R -> r))
