// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

/**
 * Transformations from Gaia photometry to other systems, from the Gaia DR3 documentation, section
 * 5.5.1 "Photometric relationships with other photometric systems":
 * https://gea.esac.esa.int/archive/documentation/GDR3/Data_processing/chap_cu5pho/cu5pho_sec_photSystem/cu5pho_ssec_photRelations.html
 *
 * Coefficients come from tables 5.8 (SDSS) and 5.9 (Johnson-Cousins, 2MASS), colour ranges and
 * their notes from table 5.10. Every relation has the form G - X = c0 + c1 x + c2 x^2 + ..., with x =
 * BP - RP, and is only valid strictly inside its colour range. The polynomials run to 4th order and
 * stop behaving physically beyond their range, so no estimate is produced outside it.
 */
object GaiaPhotometry:

  /** Polynomial in BP - RP giving G - X for a band X, valid strictly inside a colour range. */
  private final case class BandTransformation(
    band:         Band,
    coefficients: List[Double],
    minBpMinusRp: Double,
    maxBpMinusRp: Double
  ):
    // Horner's form
    def gMinusX(bpMinusRp: Double): Double =
      coefficients.foldRight(0.0)((c, acc) => c + bpMinusRp * acc)

    def inRange(bpMinusRp: Double): Boolean =
      bpMinusRp > minBpMinusRp && bpMinusRp < maxBpMinusRp

    /** X estimated from G and the colour, if the colour is in the validity range. */
    def estimate(g: BrightnessValue, bpMinusRp: Double): Option[BrightnessValue] =
      Option
        .when(inRange(bpMinusRp))(gMinusX(bpMinusRp))
        .flatMap(d => BrightnessValue.from(g.value.value - BigDecimal(d)).toOption)

  // Johnson-Cousins R, also used for Altair guide star limits. The fit runs to BP - RP 4.0, but
  // above 2.0 it only holds for M giants (table 5.10 note j).
  private val R: BandTransformation =
    BandTransformation(Band.R, List(-0.02275, 0.3961, -0.1243, -0.01396, 0.003775), 0.0, 2.0)

  // Johnson-Cousins V R, SDSS g r i and 2MASS J H Ks. Ks is stored as K. Above BP - RP 2.0, g, r and
  // R only hold for M giants (table 5.10 notes f, g and j) and the luminosity class is unknown here,
  // so their ranges stop at 2.0. V, i, J, H and Ks have no such note.
  private val Transformations: SortedMap[Band, BandTransformation] =
    SortedMap.from(
      List(
        R,
        BandTransformation(Band.V, List(-0.02704, 0.01424, -0.2156, 0.01426), -0.5, 5.0),
        BandTransformation(Band.SloanG, List(0.2199, -0.6365, -0.1548, 0.0064), 0.3, 2.0),
        BandTransformation(Band.SloanR,
                           List(-0.09837, 0.08592, 0.1907, -0.1701, 0.02263),
                           0.0,
                           2.0
        ),
        BandTransformation(Band.SloanI, List(-0.293, 0.6404, -0.09609, -0.002104), 0.5, 2.0),
        BandTransformation(Band.J, List(0.01798, 1.389, -0.09338), -0.5, 2.5),
        BandTransformation(Band.H, List(-0.1048, 2.011, -0.1758), -0.5, 2.5),
        BandTransformation(Band.K, List(-0.0981, 2.089, -0.1579), -0.5, 2.5)
      ).map(t => t.band -> t)
    )

  /**
   * Bands that `estimatedBrightnesses` can produce. `Band.K` holds the 2MASS Ks relation, which
   * differs from K by up to about 0.1 mag for red stars.
   */
  val EstimatedBands: SortedSet[Band] = Transformations.keySet

  /**
   * Colour range of the R fit. Queries for R-limited guide stars use it as a broad pre-filter; the
   * estimate itself stops at 2.0.
   */
  val MinBpMinusRp: Double = 0.0
  val MaxBpMinusRp: Double = 4.0

  /** Difference G - R for a given BP - RP colour, without checking the validity range. */
  def gMinusR(bpMinusRp: Double): Double = R.gMinusX(bpMinusRp)

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

  private def colour(bp: BrightnessValue, rp: BrightnessValue): Double =
    (bp.value.value - rp.value.value).toDouble

  // G, BP and RP from a set of brightnesses, when all three are present
  private def gaiaBands(
    brightnesses: SortedMap[Band, BrightnessValue]
  ): Option[(BrightnessValue, BrightnessValue, BrightnessValue)] =
    (brightnesses.get(Band.Gaia),
     brightnesses.get(Band.GaiaBP),
     brightnesses.get(Band.GaiaRP)
    ).tupled

  /** Johnson-Cousins R estimated from Gaia G, BP and RP, if the colour is in the validity range. */
  def johnsonCousinsR(
    g:  BrightnessValue,
    bp: BrightnessValue,
    rp: BrightnessValue
  ): Option[BrightnessValue] =
    R.estimate(g, colour(bp, rp))

  /**
   * R estimated from the G, BP and RP entries of a set of brightnesses, when all three are present
   * and the colour is in the validity range.
   */
  def estimatedR(brightnesses: SortedMap[Band, BrightnessValue]): Option[BrightnessValue] =
    gaiaBands(brightnesses).flatMap((g, bp, rp) => johnsonCousinsR(g, bp, rp))

  /**
   * Every band in `EstimatedBands` that the brightnesses lack and whose colour range contains the
   * star, estimated from their G, BP and RP entries. Measured bands are never estimated. Empty when
   * G, BP or RP is missing. `Band.K` is 2MASS Ks.
   */
  def estimatedBrightnesses(
    brightnesses: SortedMap[Band, BrightnessValue]
  ): SortedMap[Band, BrightnessValue] =
    gaiaBands(brightnesses).fold(SortedMap.empty[Band, BrightnessValue]): (g, bp, rp) =>
      val c = colour(bp, rp)
      Transformations
        .filterNot((band, _) => brightnesses.contains(band))
        .flatMap((band, t) => t.estimate(g, c).tupleLeft(band))
