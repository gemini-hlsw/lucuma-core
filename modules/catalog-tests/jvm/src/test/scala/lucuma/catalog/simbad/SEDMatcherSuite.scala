// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.syntax.eq.*
import lucuma.catalog.SEDMatcherFixture
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite

class SEDMatcherSuite extends CatsEffectSuite with SEDMatcherFixture:

  override def munitFixtures = List(sedFixture)

  test("A0V matches A0V_new"):
    val result = sedMatcher.inferSED("*", Some("A0V"), None)
    assert(result.exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V_new)))

  test("F1V matches F2V"):
    val result = sedMatcher.inferSED("*", Some("F1V"), None)
    assert(result.exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.F2V)))

  test("G4V matches G5V_new"):
    val result = sedMatcher.inferSED("*", Some("G4V"), None)
    assert(result.exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G5V_new)))

  test("MainSequence* otype with G4V spectral type matches G5V_new"):
    val result = sedMatcher.inferSED("MainSequence*", Some("G4V"), None)
    assert(result.exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G5V_new)))

  test("quasar otype returns QS0"):
    val result = sedMatcher.inferSED("QSO", None, None)
    assert(result.exists(_ === UnnormalizedSED.Quasar(QuasarSpectrum.QS0)))

  test("HII otype returns OrionNebula"):
    val result = sedMatcher.inferSED("HII", None, None)
    assert(result.exists(_ === UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula)))

  test("PN otype returns NGC7009"):
    val result = sedMatcher.inferSED("PN", None, None)
    assert(result.exists(_ === UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)))

  test("galaxy with E morphology returns Elliptical"):
    val result = sedMatcher.inferSED("G", None, Some("E3"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)))

  test("galaxy with Sa morphology returns Spiral"):
    val result = sedMatcher.inferSED("G", None, Some("Sa"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)))

  test("unknown otype returns error"):
    val result = sedMatcher.inferSED("UnknownType", Some("A0Va"), None)
    assert(result.isLeft)

  test("galaxy with Hubble stage >= 9.0 returns Spiral (irregular galaxies)"):
    val result = sedMatcher.inferSED("G", None, Some("10.0"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)))

  test("galaxy with Hubble stage 9.0 returns Spiral"):
    val result = sedMatcher.inferSED("G", None, Some("9.0"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)))

  test("star with empty spectral type returns error"):
    val result = sedMatcher.inferSED("*", Some(""), None)
    assert(result.isLeft)

  test("star with missing spectral type returns error"):
    val result = sedMatcher.inferSED("*", None, None)
    assert(result.isLeft)

  test("galaxy with S0 morphology returns Elliptical"):
    val result = sedMatcher.inferSED("G", None, Some("S0"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)))

  test("galaxy with SO morphology (capital O) returns Elliptical"):
    val result = sedMatcher.inferSED("G", None, Some("SO"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)))

  test("galaxy with SO-a morphology returns Elliptical"):
    val result = sedMatcher.inferSED("G", None, Some("SO-a"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)))

  test("galaxy with negative Hubble stage returns Elliptical"):
    val result = sedMatcher.inferSED("G", None, Some("-3.0"))
    assert(result.exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)))

  test("galaxy with invalid morphology returns error"):
    val result = sedMatcher.inferSED("G", None, Some("invalid"))
    assert(result.isLeft)
