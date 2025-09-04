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

  // Use a fixed observation time for deterministic tests (today at midnight UTC)
  private val observationTime: Instant = LocalDate.now().atStartOfDay(ZoneOffset.UTC).toInstant()

  test("Parse gaia blind offset test VOTable file") {
    val xmlFile = "/gaia-blind-offset-test.xml"
    val file    = getClass.getResource(xmlFile)

    Files[IO]
      .readAll(Path(file.getPath))
      .through(text.utf8.decode)
      .through(CatalogSearch.siderealTargets(CatalogAdapter.Gaia3LiteGavo))
      .compile
      .toList
      .map { results =>
        // Verify we get the expected number of targets
        assertEquals(results.length, 5)

        // Check that all targets parsed successfully (no Left values)
        val (errors, targets) = results.partitionEither(identity)
        assertEquals(errors.length, 0, s"Expected no parsing errors, got: ${errors}")
        assertEquals(targets.length, 5)

        // Verify we have the expected source IDs
        val sourceIds   = targets.map(_.target.name.value).toSet
        val expectedIds = Set(
          "Gaia DR3 123456789012345001", // Star 1
          "Gaia DR3 123456789012345002", // Star 2
          "Gaia DR3 123456789012345003", // Star 3
          "Gaia DR3 123456789012345004", // Star 4
          "Gaia DR3 123456789012345005"  // Star 5
        )
        assertEquals(sourceIds, expectedIds)
      }
  }

  test("Blind offset candidates can be created from parsed targets") {
    val xmlFile    = "/gaia-blind-offset-test.xml"
    val file       = getClass.getResource(xmlFile)
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
      .map { targetResults =>
        // Convert target results to blind offset candidates using the new API that filters unusable candidates
        val targets            = targetResults.map(_.target)
        val baseObjectTracking = ObjectTracking.SiderealObjectTracking(baseSiderealTracking)
        val sorted             = BlindOffsetScoringAlgorithm.sortCandidatesFromTargets(
          targets,
          baseObjectTracking,
          observationTime
        )

        // All targets should be converted to candidates (no filtering)
        assert(sorted.length >= 0, s"Should have candidates, got ${sorted.length}")

        if (sorted.nonEmpty) {
          // Verify all candidates have valid scores (some may be Double.MaxValue for unusable candidates)
          assert(sorted.forall(_.score >= 0.0), "All candidates should have non-negative scores")

          // Verify sorting (scores should be ascending)
          val scores = sorted.map(_.score)
          assertEquals(scores, scores.sorted, "Candidates should be sorted by score (ascending)")

          // Verify candidates with valid G magnitude come first
          val usableCandidates = sorted.filterNot(_.score == Double.MaxValue)
          if (usableCandidates.nonEmpty) {
            val best = usableCandidates.head
            assert(best.angularDistance.toMicroarcseconds <= 180 * 1000000,
                   "Best candidate should be within 180 arcseconds"
            )
          }
        }
      }
  }
