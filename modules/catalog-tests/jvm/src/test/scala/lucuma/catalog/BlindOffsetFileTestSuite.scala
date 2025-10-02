// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.ObjectTracking
import lucuma.core.model.SiderealTracking
import munit.CatsEffectSuite

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

class BlindOffsetFileTestSuite extends CatsEffectSuite:

  private val observationTime: Instant =
    LocalDate.of(2025, 9, 4).atStartOfDay(ZoneOffset.UTC).toInstant()

  private val xmlFile = "/gaia-blind-offset-test.xml"
  private val file    = getClass.getResource(xmlFile)

  test("Parse gaia blind offset test VOTable file"):
    val xmlFile = "/gaia-blind-offset-test.xml"
    val file    = getClass.getResource(xmlFile)

    Files[IO]
      .readAll(Path(file.getPath))
      .through(text.utf8.decode)
      .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia3LiteGavo))
      .compile
      .toList
      .map: results =>
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

  test("Blind offset candidates can be created from parsed targets"):

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

    Files[IO]
      .readAll(Path(file.getPath))
      .through(text.utf8.decode)
      .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia3LiteGavo))
      .compile
      .toList
      .map(_.collect { case Right(targetResult) => targetResult })
      .map: targetResults =>
        val baseObjectTracking = ObjectTracking.SiderealObjectTracking(baseSiderealTracking)
        val sorted             = BlindOffsets.analysis(
          targetResults,
          baseObjectTracking,
          observationTime
        )

        assert(sorted.nonEmpty)
        assert(sorted.forall(_.score >= 0.0))

        val scores = sorted.map(_.score)
        // scores are sorted
        assertEquals(scores, scores.sorted)

        val usableCandidates = sorted.filterNot(_.score == Double.MaxValue)
        assert(usableCandidates.nonEmpty)
        val best             = usableCandidates.head
        assert(best.distance.toDoubleDegrees <= 180)
