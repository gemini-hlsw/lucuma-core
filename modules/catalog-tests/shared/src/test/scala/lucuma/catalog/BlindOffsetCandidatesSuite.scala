// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import lucuma.catalog.simbad.GravityTableConfig
import lucuma.catalog.simbad.SEDDataConfig
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.simbad.StellarLibraryConfig
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import munit.FunSuite

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset
import scala.collection.immutable.SortedMap

class BlindOffsetCandidatesSuite extends FunSuite:

  private val observationTime: Instant =
    LocalDate.of(2025, 9, 4).atStartOfDay(ZoneOffset.UTC).toInstant()

  // A one-spectrum library: G2V, Teff 5778 K from the Malkov fit, log g 4.4
  private val sedMatcher: SEDMatcher =
    SEDMatcher.fromConfig(
      SEDDataConfig(
        StellarLibraryConfig(List(StellarLibrarySpectrum.G2V -> (List("V"), List("G2")))),
        GravityTableConfig(List(40.0 -> Map("V" -> 4.4), 50.0 -> Map("V" -> 4.6)))
      )
    )

  private def coords(ra: String, dec: String): Coordinates =
    (RightAscension.fromStringHMS.getOption(ra), Declination.fromStringSignedDMS.getOption(dec))
      .mapN(Coordinates.apply)
      .getOrElse(Coordinates.Zero)

  private val baseCoords = coords("05:35:17.3", "+22:00:52.2")

  private def gaiaStar(
    name: String,
    c:    Coordinates,
    g:    Double,
    bp:   Double,
    rp:   Double
  ): CatalogTargetResult =
    def mag(d: Double) =
      Band.Gaia.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(BigDecimal(d)))
    CatalogTargetResult(
      Target.Sidereal(
        name = eu.timepit.refined.types.string.NonEmptyString.unsafeFrom(name),
        tracking = SiderealTracking(c, Epoch.J2000, None, None, None),
        sourceProfile = SourceProfile.Point(
          SpectralDefinition.BandNormalized(
            None,
            SortedMap[Band, BrightnessMeasure[Integrated]](
              Band.Gaia   -> mag(g),
              Band.GaiaBP -> mag(bp),
              Band.GaiaRP -> mag(rp)
            )
          )
        ),
        catalogInfo = None
      ),
      None
    )

  private def analyse(
    results: List[CatalogTargetResult],
    limits:  BlindOffsetLimits,
    params:  Map[Long, GaiaStellarParameters] = Map.empty
  ) =
    BlindOffsets.analysis(results, sedMatcher, limits, baseCoords, observationTime, params)

  test("score matches the pyexplore formula"):
    // Gmos: V optimal 13. BP-RP = 1 gives V = G + 0.02704 - 0.01424 + 0.2156 - 0.01426
    val star      = gaiaStar("s", coords("05:35:17.3", "+22:01:52.2"), 15.0, 15.5, 14.5)
    val candidate = analyse(List(star), BlindOffsetLimits.Gmos).head
    val v         = 15.0 + 0.02704 - 0.01424 + 0.2156 - 0.01426
    val expected  = math.sqrt(math.pow((v - 13.0) / 5.0, 2) + math.pow(60.0 / 30.0, 2))
    assertEqualsDouble(candidate.score.get.toDouble, expected, 1e-3)

  test("candidates outside the limits are rejected but kept, sorted last"):
    val c      = coords("05:35:18.0", "+22:00:53.0")
    val ok     = gaiaStar("ok", c, 14.0, 14.5, 13.5)
    val bright = gaiaStar("bright", c, 9.0, 9.5, 8.5)
    val faint  = gaiaStar("faint", c, 19.5, 20.0, 19.0)

    val sorted = analyse(List(bright, faint, ok), BlindOffsetLimits.Gmos)
    assertEquals(sorted.map(_.sourceId.value).head, "ok")
    assertEquals(
      sorted.map(c => c.sourceId.value -> c.rejection).toMap,
      Map(
        "ok"     -> None,
        "bright" -> Some(BlindOffsetRejection.TooBright),
        "faint"  -> Some(BlindOffsetRejection.TooFaint)
      )
    )
    // Rejected candidates still have a score
    assert(sorted.forall(_.score.isDefined))

  test("estimated magnitudes are attached for every band in range"):
    val star   = gaiaStar("s", coords("05:35:18.0", "+22:00:53.0"), 14.0, 14.5, 13.5)
    val result = analyse(List(star), BlindOffsetLimits.Gmos).head
    val bands  = SourceProfile.integratedBrightnesses
      .getOption(result.catalogResult.target.sourceProfile)
      .map(_.keySet)
      .getOrElse(Set.empty)
    assertEquals(
      bands,
      Set(Band.Gaia, Band.GaiaBP, Band.GaiaRP) ++ GaiaPhotometry.EstimatedBands
    )

  test("ties are broken by distance, then source id, whatever the input order"):
    val near     = coords("05:35:18.0", "+22:00:53.0")
    val far      = coords("05:35:30.0", "+22:01:40.0")
    // BP-RP = 3.5 has no H estimate, so none of these has a score
    val stars    = List(
      gaiaStar("b", near, 14.0, 16.0, 12.5),
      gaiaStar("far", far, 14.0, 16.0, 12.5),
      gaiaStar("a", near, 14.0, 16.0, 12.5)
    )
    val expected = List("a", "b", "far")
    assertEquals(analyse(stars, BlindOffsetLimits.Gnirs).map(_.sourceId.value), expected)
    assertEquals(analyse(stars.reverse, BlindOffsetLimits.Gnirs).map(_.sourceId.value), expected)
