// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.effect.IO
import cats.syntax.all.*
import lucuma.catalog.SEDMatcherFixture
import lucuma.catalog.SimbadEntry
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite

/**
 * Test suite for SED matching comparing to the python reference implementation.
 */
class SimbadSEDMatcherSuite extends CatsEffectSuite with SEDMatcherFixture:

  override def munitFixtures = List(sedFixture)

  private def filenameToSED(filename: String): Option[UnnormalizedSED] =
    filename match
      case "QSO.sed"        => Some(UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
      case "HII.sed"        => Some(UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
      case "PN.sed"         => Some(UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
      case "Elliptical.sed" => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case "Spiral.sed"     => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
      case stellar          =>
        val spectrumTag = stellar.stripSuffix(".nm")
        StellarLibrarySpectrum.values
          .find(_.tag === spectrumTag)
          .map(UnnormalizedSED.StellarLibrary(_))

  private def stellarBaseType(sed: UnnormalizedSED): Option[String] =
    sed match
      case UnnormalizedSED.StellarLibrary(spectrum) =>
        spectrum.toString.stripSuffix("_new").some
      case _                                        =>
        none

  private def sedEquivalent(p: UnnormalizedSED, s: UnnormalizedSED): Boolean =
    (stellarBaseType(p), stellarBaseType(s)) match
      case (Some(pBase), Some(sBase)) => pBase === sBase
      case _                          => p === s

  /**
   * Checks if two stellar SEDs scored within 1% of each other, indicating a tiebreak difference
   *
   * Uses the same normalized Euclidean distance as SEDMatcher.scoreSpectrum:
   * {{{score = sqrt((dT / dT_max)^2 + (dg / dg_max)^2)}}} where dT_max = 10% of target temperature
   * dg_max = 0.5 dex.
   *
   * Two spectra are a "tie" when |score_p - score_s| / max(score_p, score_s) < 0.01.
   */
  private def tiedSED(
    pythonSED: UnnormalizedSED,
    scalaSED:  UnnormalizedSED,
    entry:     SimbadEntry
  ): Boolean =
    (pythonSED, scalaSED) match
      case (UnnormalizedSED.StellarLibrary(pSpectrum), UnnormalizedSED.StellarLibrary(sSpectrum)) =>
        val cleaned = entry.spectralType.replaceAll("[():]", "")
        SpectralTypeParsers.spectralType
          .parse(cleaned)
          .toOption
          .flatMap:
            case (_, (lum, temp)) =>
              sedPhysics
                .calculateParameters(lum, temp)
                .flatMap: targetParams =>
                  val pParams = sedLibrary.params.get(pSpectrum)
                  val sParams = sedLibrary.params.get(sSpectrum)

                  (pParams, sParams).mapN: (pp, sp) =>
                    val dtMax = 0.1 * targetParams.temp.value
                    val dgMax = 0.5

                    def score(libParams: StellarPhysics.StellarParameters): Double =
                      val dt = (libParams.temp.value - targetParams.temp.value).toDouble / dtMax
                      val dg = (libParams.logG - targetParams.logG) / dgMax
                      math.sqrt(dt * dt + dg * dg)

                    val pScore = score(pp)
                    val sScore = score(sp)

                    math.abs(pScore - sScore) / math.max(pScore, sScore) < 0.01
          .getOrElse(false)
      case _                                                                                      => false

  test("sanity test"):
    testData.map(_.length > 8000).assert

  test("validate against Python"):
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
                  s"${entry.mainId} (${entry.spectralType}): Python=${p}, Scala=${s}" :: acc.errors
                )
              case (Some(p), None)                            =>
                acc.copy(errors =
                  s"${entry.mainId}: Python matched ($p), Scala failed" :: acc.errors
                )
              case (None, Some(s))                            =>
                acc.copy(errors =
                  s"${entry.mainId}: Python failed, Scala matched ($s)" :: acc.errors
                )

      assert(result.errors.isEmpty)
      assertEquals(result.matches, testData.length)
