// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.AgsAnalysis.Usable
import lucuma.catalog.GuideStarCatalogResult
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.FieldId
import lucuma.catalog.votable.Ucd
import lucuma.catalog.votable.VoTableParser
import lucuma.core.enums.Band
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.geom.Area
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target

import scala.collection.immutable.SortedMap

class RuweRankingSuite extends munit.FunSuite:

  private def candidate(id: Long, g: Double, ruwe: Option[NonNegBigDecimal]): GuideStarCandidate =
    GuideStarCandidate(
      GuideStarId.GaiaDr3(id),
      SiderealTracking.const(Coordinates.Zero),
      SortedMap[Band, BrightnessValue](Band.Gaia -> BrightnessValue.unsafeFrom(g)),
      ruwe
    )

  private def usable(
    gs:         GuideStarCandidate,
    vignetting: Long = 0,
    speed:      GuideSpeed = GuideSpeed.Fast,
    quality:    AgsGuideQuality = AgsGuideQuality.DeliversRequestedIq
  ): Usable =
    Usable(GuideProbe.GmosOIWFS,
           gs,
           speed,
           quality,
           Angle.Angle0,
           Area.fromMicroarcsecondsSquared.getOption(vignetting).get
    )

  private def best(us: Usable*): Usable = us.toList.sorted(using Usable.rankOrdering).head

  private def r(s: String): Option[NonNegBigDecimal] = Some(
    NonNegBigDecimal.unsafeFrom(BigDecimal(s))
  )

  test("RUWE tiers, missing is worst"):
    List(
      "0.8"  -> 0,
      "1.4"  -> 0,
      "1.41" -> 1,
      "2.79" -> 1,
      "2.8"  -> 1,
      "2.81" -> 2,
      "4.2"  -> 2,
      "4.21" -> 3,
      "25"   -> 3
    ).foreach: (ruwe, tier) =>
      assertEquals(Usable.ruweTier(r(ruwe)), tier, ruwe)
    assertEquals(Usable.ruweTier(None), 3)

  test("a better RUWE tier beats a brighter star"):
    val good   = usable(candidate(1, 15.0, r("1.0")))
    val bright = usable(candidate(2, 12.0, r("1.5")))
    assertEquals(best(bright, good), good)

  test("within a tier the brighter star wins"):
    val good   = usable(candidate(1, 15.0, r("1.0")))
    val bright = usable(candidate(2, 12.0, r("1.3")))
    assertEquals(best(good, bright), bright)

  test("missing RUWE shares the worst tier"):
    val missing = usable(candidate(1, 12.0, None))
    val poor    = usable(candidate(2, 15.0, r("6.0")))
    assertEquals(best(poor, missing), missing)
    assertEquals(best(missing, usable(candidate(3, 15.0, r("4.0")))).target.id,
                 GuideStarId.GaiaDr3(3)
    )

  test("guide speed beats RUWE"):
    val fastPoor = usable(candidate(1, 12.0, r("5.0")))
    val slowGood = usable(candidate(2, 12.0, r("1.0")), speed = GuideSpeed.Slow)
    assertEquals(best(slowGood, fastPoor), fastPoor)

  test("quality beats RUWE"):
    val goodIq   = usable(candidate(1, 12.0, r("5.0")))
    val degraded =
      usable(candidate(2, 12.0, r("1.0")), quality = AgsGuideQuality.PossibleIqDegradation)
    assertEquals(best(degraded, goodIq), goodIq)

  test("vignetting beats RUWE"):
    val clearPoor     = usable(candidate(1, 12.0, r("5.0")))
    val vignettedGood = usable(candidate(2, 12.0, r("1.0")), vignetting = 1000)
    assertEquals(best(vignettedGood, clearPoor), clearPoor)

  test("parseRuwe reads the column by name, ignoring case and UCD"):
    def parse(adapter: CatalogAdapter, entries: (FieldId, String)*): Option[NonNegBigDecimal] =
      VoTableParser.parseRuwe(adapter, entries.toMap)

    val gaia     = CatalogAdapter.Gaia3LiteGavo
    val plain    = FieldId.unsafeFrom("ruwe")
    val withUcd  = FieldId.unsafeFrom("ruwe", Ucd.unsafeFromString("stat.fit.goodness"))
    val upper    = FieldId.unsafeFrom("RUWE")
    val other    = FieldId.unsafeFrom("parallax")
    val expected = Some(NonNegBigDecimal.unsafeFrom(BigDecimal(1.25)))

    assertEquals(parse(gaia, plain -> "1.25"), expected)
    assertEquals(parse(gaia, withUcd -> " 1.25 "), expected)
    assertEquals(parse(gaia, upper -> "1.25"), expected)
    assertEquals(parse(gaia, plain -> "   "), None)
    assertEquals(parse(gaia, plain -> "NaN"), None)
    assertEquals(parse(gaia, plain -> "Infinity"), None)
    assertEquals(parse(gaia, plain -> "junk"), None)
    assertEquals(parse(gaia, plain -> "-1.0"), None)
    assertEquals(parse(gaia, other -> "1.25"), None)

  test("fromCatalogResult keeps RUWE"):
    val target = Target.Sidereal(
      NonEmptyString.unsafeFrom("Gaia DR3 42"),
      SiderealTracking.const(Coordinates.Zero),
      SourceProfile.Point(SpectralDefinition.BandNormalized(None, SortedMap.empty)),
      None
    )
    val c      = GuideStarCandidate.fromCatalogResult(GuideStarCatalogResult(target, r("2.0")))
    assertEquals(c.id, GuideStarId.GaiaDr3(42))
    assertEquals(c.ruwe, r("2.0"))
