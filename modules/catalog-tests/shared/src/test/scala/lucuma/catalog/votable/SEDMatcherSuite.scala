// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.UnnormalizedSED
import munit.FunSuite

class SEDMatcherSuite extends FunSuite:

  test("A0V matches A0V_new (Python-preferred calspec)") {
    val result = SEDMatcher.inferSED("*", Some("A0V"), None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V_new)
    )
  }

  test("F1V matches F2V (closest available)") {
    val result = SEDMatcher.inferSED("*", Some("F1V"), None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.F2V)
    )
  }

  test("quasar otype returns QS0") {
    val result = SEDMatcher.inferSED("QSO", None, None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Quasar(QuasarSpectrum.QS0)
    )
  }

  test("HII otype returns OrionNebula") {
    val result = SEDMatcher.inferSED("HII", None, None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula)
    )
  }

  test("PN otype returns NGC7009") {
    val result = SEDMatcher.inferSED("PN", None, None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)
    )
  }

  test("galaxy with E morphology returns Elliptical") {
    val result = SEDMatcher.inferSED("G", None, Some("E3"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)
    )
  }

  test("galaxy with Sa morphology returns Spiral") {
    val result = SEDMatcher.inferSED("G", None, Some("Sa"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)
    )
  }

  test("unknown otype returns error") {
    val result = SEDMatcher.inferSED("UnknownType", Some("A0Va"), None)
    assert(result.isLeft)
  }
