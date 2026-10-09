// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import munit.CatsEffectSuite

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

class BlindOffsetFileTestSuite extends CatsEffectSuite with SEDMatcherFixture:

  override def munitFixtures = List(sedFixture)

  private val observationTime: Instant =
    LocalDate.of(2025, 9, 4).atStartOfDay(ZoneOffset.UTC).toInstant()

  private def parsed(adapter: CatalogAdapter) =
    readClassLoaderResource[IO]("gaia-blind-offset-test.xml")
      .through(text.utf8.decode)
      .through(CatalogSearch.siderealTargets(adapter))
      .compile
      .toList

  test("Parse gaia blind offset test VOTable file"):
    parsed(CatalogAdapter.Gaia3LiteGavo).map: results =>
      assertEquals(results.length, 5)

      val (errors, targets) = results.partitionEither(identity)
      assertEquals(errors.length, 0)
      assertEquals(targets.length, 5)

      val sourceIds   = targets.map(_.target.name.value).toSet
      val expectedIds = Set(
        "Gaia DR3 123456789012345001",
        "Gaia DR3 123456789012345002",
        "Gaia DR3 123456789012345003",
        "Gaia DR3 123456789012345004",
        "Gaia DR3 123456789012345005"
      )
      assertEquals(sourceIds, expectedIds)

  test("Candidates with G only have no magnitude estimate and are rejected"):
    val baseCoords = (
      RightAscension.fromStringHMS.getOption("05:35:17.3"),
      Declination.fromStringSignedDMS.getOption("+22:00:52.0")
    ).mapN(Coordinates.apply).getOrElse(Coordinates.Zero)

    val baseSiderealTracking = SiderealTracking(
      baseCoordinates = baseCoords,
      epoch = Epoch.J2000,
      properMotion = None,
      radialVelocity = None,
      parallax = None
    )

    parsed(CatalogAdapter.Gaia3LiteGavo)
      .map(_.collect { case Right(targetResult) => targetResult })
      .map: targetResults =>
        val sorted = BlindOffsets.analysis(
          targetResults,
          sedMatcher,
          BlindOffsetLimits.Gmos,
          baseSiderealTracking,
          observationTime,
          Map.empty
        )

        assertEquals(sorted.length, 5)
        assert(sorted.forall(_.rejection.contains(BlindOffsetRejection.NoMagnitudeEstimate)))
        assert(sorted.forall(_.score.isEmpty))
        assert(sorted.forall(_.distance.toDoubleDegrees <= 180))
