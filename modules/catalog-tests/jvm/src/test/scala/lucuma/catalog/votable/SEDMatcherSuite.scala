// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.Resource
import lucuma.core.enums.GalaxySpectrum
import lucuma.core.enums.HIIRegionSpectrum
import lucuma.core.enums.PlanetaryNebulaSpectrum
import lucuma.core.enums.QuasarSpectrum
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.model.UnnormalizedSED
import munit.CatsEffectSuite

class SEDMatcherSuite extends CatsEffectSuite:

  val matcherFixture = ResourceSuiteLocalFixture(
    "matcher",
    Resource.eval(SEDDataLoader.load.map { config =>
      val physics = new StellarPhysics(config.gravityTable)
      val library = new StellarLibraryParameters(config.stellarLibrary, physics)
      new SEDMatcher(library, physics)
    })
  )

  override def munitFixtures = List(matcherFixture)

  def matcher: SEDMatcher = matcherFixture()

  test("A0V matches A0V_new (Python-preferred calspec)") {
    val result = matcher.inferSED("*", Some("A0V"), None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V_new)
    )
  }

  test("F1V matches F2V (closest available)") {
    val result = matcher.inferSED("*", Some("F1V"), None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.F2V)
    )
  }

  test("quasar otype returns QS0") {
    val result = matcher.inferSED("QSO", None, None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Quasar(QuasarSpectrum.QS0)
    )
  }

  test("HII otype returns OrionNebula") {
    val result = matcher.inferSED("HII", None, None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula)
    )
  }

  test("PN otype returns NGC7009") {
    val result = matcher.inferSED("PN", None, None)
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)
    )
  }

  test("galaxy with E morphology returns Elliptical") {
    val result = matcher.inferSED("G", None, Some("E3"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)
    )
  }

  test("galaxy with Sa morphology returns Spiral") {
    val result = matcher.inferSED("G", None, Some("Sa"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)
    )
  }

  test("unknown otype returns error") {
    val result = matcher.inferSED("UnknownType", Some("A0Va"), None)
    assert(result.isLeft)
  }

  test("galaxy with Hubble stage >= 9.0 returns Spiral (irregular galaxies)") {
    val result = matcher.inferSED("G", None, Some("10.0"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)
    )
  }

  test("galaxy with Hubble stage 9.0 returns Spiral") {
    val result = matcher.inferSED("G", None, Some("9.0"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)
    )
  }

  test("star with empty spectral type returns error") {
    val result = matcher.inferSED("*", Some(""), None)
    assert(result.isLeft)
  }

  test("star with missing spectral type returns error") {
    val result = matcher.inferSED("*", None, None)
    assert(result.isLeft)
  }

  test("galaxy with S0 morphology returns Elliptical") {
    val result = matcher.inferSED("G", None, Some("S0"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)
    )
  }

  test("galaxy with negative Hubble stage returns Elliptical") {
    val result = matcher.inferSED("G", None, Some("-3.0"))
    assert(result.isRight)
    assertEquals(
      result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)
    )
  }

  test("galaxy with invalid morphology returns error") {
    val result = matcher.inferSED("G", None, Some("invalid"))
    assert(result.isLeft)
  }
