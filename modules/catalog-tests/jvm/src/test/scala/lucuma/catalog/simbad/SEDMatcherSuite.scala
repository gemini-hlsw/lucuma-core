// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.syntax.eq.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
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

  test("match on Teff and log g"):
    // Values the library derives for A0V and G2V from their spectral types (Malkov et al. 2020)
    assertEquals(sedMatcher.matchStellarParameters(11531.withUnit[Kelvin], 4.07).map(_.tag),
                 Some("A0V_calspec")
    )
    assert(
      sedMatcher.matchStellarParameters(5199.withUnit[Kelvin], 4.4).exists(_.tag.startsWith("G2V"))
    )
    // Nothing in the library is this hot and compact
    assertEquals(sedMatcher.matchStellarParameters(100000.withUnit[Kelvin], 8.0), None)

  test("SED matcher validation"):
    assert(
      sedMatcher
        .inferSED("*", Some("A0V"), None)
        .exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V_new))
    )
    assert(
      sedMatcher
        .inferSED("*", Some("F1V"), None)
        .exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.F2V))
    )
    assert(
      sedMatcher
        .inferSED("*", Some("G4V"), None)
        .exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G5V_new))
    )
    assert(sedMatcher.inferSED("*", Some(""), None).isLeft)
    assert(sedMatcher.inferSED("*", None, None).isLeft)

    assert(
      sedMatcher
        .inferSED("MainSequence*", Some("G4V"), None)
        .exists(_ === UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G5V_new))
    )
    assert(
      sedMatcher
        .inferSED("QSO", None, None)
        .exists(_ === UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
    )
    assert(
      sedMatcher
        .inferSED("HII", None, None)
        .exists(_ === UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
    )
    assert(
      sedMatcher
        .inferSED("PN", None, None)
        .exists(_ === UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
    )
    assert(sedMatcher.inferSED("UnknownType", Some("A0Va"), None).isLeft)

    assert(
      sedMatcher
        .inferSED("G", None, Some("E3"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    )
    assert(
      sedMatcher
        .inferSED("G", None, Some("10.0"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
    )
    assert(
      sedMatcher
        .inferSED("G", None, Some("9.0"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
    )
    assert(
      sedMatcher
        .inferSED("G", None, Some("S0"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    )
    assert(
      sedMatcher
        .inferSED("G", None, Some("SO"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    )
    assert(
      sedMatcher
        .inferSED("G", None, Some("SO-a"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    )
    assert(
      sedMatcher
        .inferSED("G", None, Some("-3.0"))
        .exists(_ === UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    )
    assert(sedMatcher.inferSED("G", None, Some("invalid")).isLeft)
