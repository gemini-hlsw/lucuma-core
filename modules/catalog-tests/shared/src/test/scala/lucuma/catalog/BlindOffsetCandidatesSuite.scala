// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.syntax.all.*
import lucuma.catalog.simbad.GravityTableConfig
import lucuma.catalog.simbad.SEDDataConfig
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.simbad.StellarLibraryConfig
import lucuma.core.enums.Band
import lucuma.core.enums.Instrument
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
import lucuma.core.model.UnnormalizedSED
import lucuma.core.refined.auto.*
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
    name:   String,
    c:      Coordinates,
    g:      Double,
    bp:     Double,
    rp:     Double,
    params: Option[StellarParameters] = None
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
      None,
      params
    )

  private def analyse(results: List[CatalogTargetResult], limits: BlindOffsetLimits) =
    BlindOffsets.analysis(results, sedMatcher, limits, baseCoords, observationTime)

  test("limits exist for every instrument that supports blind offsets"):
    assertEquals(BlindOffsetLimits.forInstrument(Instrument.GmosNorth),
                 Some(BlindOffsetLimits.Gmos)
    )
    assertEquals(BlindOffsetLimits.forInstrument(Instrument.GmosSouth),
                 Some(BlindOffsetLimits.Gmos)
    )
    assertEquals(BlindOffsetLimits.forInstrument(Instrument.Gnirs).map(_.band), Some(Band.H))
    assertEquals(BlindOffsetLimits.forInstrument(Instrument.MaroonX), None)
    assertEquals(BlindOffsetLimits.forInstrument(Instrument.Niri), None)

  test("closer candidates score better at equal brightness"):
    val near = gaiaStar("near", coords("05:35:18.0", "+22:00:53.0"), 13.5, 14.0, 13.0)
    val far  = gaiaStar("far", coords("05:35:30.0", "+22:01:40.0"), 13.5, 14.0, 13.0)

    val sorted = analyse(List(far, near), BlindOffsetLimits.Gmos)
    assertEquals(sorted.map(_.sourceId.value), List("near", "far"))
    assert(sorted.forall(_.isUsable))
    assert(sorted.forall(_.score.exists(_ > 0)))

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

  test("a colour outside the transformation range gives no estimate"):
    // BP-RP = 3.5 is outside the H range (-0.5, 2.5) but inside V (-0.5, 5.0)
    val red = gaiaStar("red", coords("05:35:18.0", "+22:00:53.0"), 14.0, 16.0, 12.5)

    val gnirs = analyse(List(red), BlindOffsetLimits.Gnirs).head
    assertEquals(gnirs.rejection, Some(BlindOffsetRejection.NoMagnitudeEstimate))
    assertEquals(gnirs.score, None)
    assertEquals(gnirs.selectionBrightness, None)

    val gmos = analyse(List(red), BlindOffsetLimits.Gmos).head
    assert(gmos.selectionBrightness.isDefined)

  test("estimated magnitudes are attached for every band in range"):
    val star   = gaiaStar("s", coords("05:35:18.0", "+22:00:53.0"), 14.0, 14.5, 13.5)
    val result = analyse(List(star), BlindOffsetLimits.Gmos).head
    val bands  = SourceProfile.integratedBrightnesses
      .getOption(result.catalogResult.target.sourceProfile)
      .map(_.keySet)
      .getOrElse(Set.empty)
    assertEquals(
      bands,
      Set(Band.Gaia, Band.GaiaBP, Band.GaiaRP) ++ GaiaPhotometry.Transformations.keySet
    )

  test("SED comes from the catalog Teff and log g when a library spectrum matches"):
    val sunLike = gaiaStar(
      "sun",
      coords("05:35:18.0", "+22:00:53.0"),
      14.0,
      14.5,
      13.5,
      Some(StellarParameters(5750, 4.4, StellarParametersSource.GspPhot))
    )
    val sed     = analyse(List(sunLike), BlindOffsetLimits.Gmos).head.catalogResult.target.sourceProfile
    assertEquals(
      SourceProfile.unnormalizedSED.getOption(sed).flatten,
      Some(UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G2V))
    )

  test("SED is a flat power law without stellar parameters or without a match"):
    val c      = coords("05:35:18.0", "+22:00:53.0")
    val none   = gaiaStar("none", c, 14.0, 14.5, 13.5)
    val hot    = gaiaStar("hot",
                       c,
                       14.0,
                       14.5,
                       13.5,
                       Some(StellarParameters(30000, 4.0, StellarParametersSource.EspHs))
    )
    val result = analyse(List(none, hot), BlindOffsetLimits.Gmos)
    result.foreach: r =>
      assertEquals(
        SourceProfile.unnormalizedSED.getOption(r.catalogResult.target.sourceProfile).flatten,
        Some(UnnormalizedSED.PowerLaw(BigDecimal(0)))
      )

  test("candidates without Gaia photometry sort last with no score"):
    val c    = coords("05:35:18.0", "+22:00:53.0")
    val bare = CatalogTargetResult(
      Target.Sidereal(
        name = "bare".refined,
        tracking = SiderealTracking(c, Epoch.J2000, None, None, None),
        sourceProfile =
          SourceProfile.Point(SpectralDefinition.BandNormalized(None, SortedMap.empty)),
        catalogInfo = None
      ),
      None
    )
    val ok   = gaiaStar("ok", c, 14.0, 14.5, 13.5)

    val sorted = analyse(List(bare, ok), BlindOffsetLimits.Gmos)
    assertEquals(sorted.map(_.sourceId.value), List("ok", "bare"))
    assertEquals(sorted.last.score, None)
    assertEquals(sorted.last.rejection, Some(BlindOffsetRejection.NoMagnitudeEstimate))
