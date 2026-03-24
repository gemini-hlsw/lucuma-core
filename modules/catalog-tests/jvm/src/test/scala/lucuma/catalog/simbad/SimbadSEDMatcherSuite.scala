// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.syntax.all.*
import lucuma.catalog.SEDMatcherFixture
import lucuma.catalog.SimbadData.*
import lucuma.catalog.SimbadEntry
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite

class SimbadSEDMatcherSuite extends CatsEffectSuite with SEDMatcherFixture:

  override def munitFixtures = List(sedFixture)

  private def tiedSED(
    pythonSED: UnnormalizedSED,
    scalaSED:  UnnormalizedSED,
    entry:     SimbadEntry
  ): Boolean =
    tiedScore(entry.spectralType, pythonSED, scalaSED, sedPhysics, sedLibrary)

  test("sanity test"):
    testData.map(_.length > 8000).assert

  test("validate against python"):
    (testData, expectedOutput).mapN: (testData, expectedOutput) =>
      val inputData = testData.fproductLeft(_.mainId).toMap

      case class ValidationResult(matches: Int, ties: Int, errors: List[String])

      object ValidationResult:
        def empty: ValidationResult = ValidationResult(0, 0, Nil)

      val result = expectedOutput.foldLeft(ValidationResult.empty): (acc, expected) =>
        inputData
          .get(expected.main_id)
          .fold(acc): entry =>
            val morphTypeOpt    = Some(entry.morphType).filter(_.nonEmpty)
            val spectralTypeOpt = Some(entry.spectralType).filter(_.nonEmpty)
            val scalaSED        = sedMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt).toOption
            val pythonSED       = expected.filename.flatMap(filenameToSED)

            (pythonSED, scalaSED) match
              case (None, None)                               =>
                acc.copy(matches = acc.matches + 1)
              case (Some(p), Some(s)) if sedEquivalent(p, s)  =>
                acc.copy(matches = acc.matches + 1)
              case (Some(p), Some(s)) if tiedSED(p, s, entry) =>
                acc.copy(matches = acc.matches + 1, ties = acc.ties + 1)
              case (Some(p), Some(s))                         =>
                acc.copy(errors =
                  s"${entry.mainId} (${entry.spectralType}): python=${p}, scala=${s}" :: acc.errors
                )
              case (Some(p), None)                            =>
                acc.copy(errors = s"${entry.mainId}: python ($p), scala failed" :: acc.errors)
              case (None, Some(s))                            =>
                acc.copy(errors = s"${entry.mainId}: python failed, scala ($s)" :: acc.errors)

      assert(result.errors.isEmpty)
      assertEquals(result.matches, testData.length)
