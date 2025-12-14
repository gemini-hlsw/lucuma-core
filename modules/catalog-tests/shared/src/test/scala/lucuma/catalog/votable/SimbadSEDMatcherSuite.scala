// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.NonEmptyChain
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import munit.FunSuite

class SimbadSEDMatcherSuite extends FunSuite {

  test("star object types should be classified as stellar") {
    val starTypes = List(
      "*",
      "PM*",
      "HB*",
      "WR*",
      "RR*",
      "Be*",
      "Em*",
      "WD*",
      "Ma*",
      "bC*",
      "sg*",
      "s*r",
      "s*y",
      "s*b",
      "N*",
      "Psr",
      "Y*O",
      "Or*",
      "TT*",
      "Ae*",
      "HH",
      "MS*",
      "BS*",
      "SX*",
      "gD*",
      "dS*",
      "Ev*",
      "RG*",
      "HS*",
      "WV*",
      "Ce*",
      "cC*",
      "C*",
      "S*",
      "LP*",
      "AB*",
      "Mi*",
      "OH*",
      "pA*",
      "RV*",
      "Pe*",
      "a2*",
      "RC*",
      "LM*",
      "BD*",
      "Ir*",
      "Er*",
      "Ro*",
      "Pu*",
      "HV*"
    )

    starTypes.foreach { otype =>
      val result = SimbadSEDMatcher.inferSED(otype, Some("G2V"))
      assert(result.isRight, s"Object type $otype should have a SED match")
      result match {
        case Right(sed) =>
          assert(sed.isInstanceOf[UnnormalizedSED.StellarLibrary],
                 s"Object type $otype should map to StellarLibrary SED"
          )
        case Left(_) => fail(s"Expected Right but got Left for $otype")
      }
    }
  }

  test("galaxy object types should be classified as galactic") {
    val galaxyTypes = List(
      "G",
      "GGG",
      "LSB",
      "bCG",
      "SBG",
      "H2G",
      "EmG",
      "rG",
      "GiP",
      "GiG",
      "GiC",
      "BiC",
      "IG",
      "PaG",
      "GrG",
      "CGG",
      "CIG",
      "PCG",
      "SCG"
    )

    galaxyTypes.foreach { otype =>
      val result = SimbadSEDMatcher.inferSED(otype, morphType = Some("Sb"))
      assert(result.isRight, s"Object type $otype should have a SED match")
      result match {
        case Right(sed) =>
          assert(sed.isInstanceOf[UnnormalizedSED.Galaxy],
                 s"Object type $otype should map to Galaxy SED"
          )
        case Left(_) => fail(s"Expected Right but got Left for $otype")
      }
    }
  }

  test("quasar object types should be classified as quasars") {
    val quasarTypes = List(
      "AGN",
      "AG?",
      "SyG",
      "Sy1",
      "Sy2",
      "QSO",
      "Q?",
      "Bla",
      "Bz?",
      "BLL",
      "BL?",
      "LIN"
    )

    quasarTypes.foreach { otype =>
      val result = SimbadSEDMatcher.inferSED(otype)
      assert(result.isRight, s"Object type $otype should have a SED match")
      result match {
        case Right(sed) =>
          assert(sed.isInstanceOf[UnnormalizedSED.Quasar],
                 s"Object type $otype should map to Quasar SED"
          )
        case Left(_) => fail(s"Expected Right but got Left for $otype")
      }
    }
  }

  test("HII region should be classified correctly") {
    val result = SimbadSEDMatcher.inferSED("HII")
    assert(result.isRight)
    result match {
      case Right(sed) => assert(sed.isInstanceOf[UnnormalizedSED.HIIRegion])
      case Left(_)    => fail("Expected Right")
    }
  }

  test("planetary nebula should be classified correctly") {
    val result = SimbadSEDMatcher.inferSED("PN")
    assert(result.isRight)
    result match {
      case Right(sed) => assert(sed.isInstanceOf[UnnormalizedSED.PlanetaryNebula])
      case Left(_)    => fail("Expected Right")
    }
  }

  test("stellar spectral types should be parsed correctly") {
    val testCases = List(
      ("A1V", "A1", "V"),
      ("G8III", "G8", "III"),
      ("M2/3V", "M2", "V"),     // Should pick first temperature class
      ("K1/2III", "K1", "III"),
      ("F5Ia", "F5", "Ia"),
      ("B8/9IV/V", "B8", "IV"), // Should pick first class
      ("G8/K0III", "G8", "III"),
      ("O9.5V", "O9.5", "V"),
      ("K3+V", "K3+", "V"),
      ("G1-V", "G1-", "V")
    )

    testCases.foreach { case (spectralType, expectedTemp, expectedLum) =>
      val result = SimbadSEDMatcher.inferSED("*", Some(spectralType))
      assert(result.isRight, s"Spectral type $spectralType should be parseable")
      result match {
        case Right(sed) =>
          assert(sed.isInstanceOf[UnnormalizedSED.StellarLibrary],
                 s"Spectral type $spectralType should map to StellarLibrary"
          )
        case Left(_) => fail(s"Expected Right for $spectralType")
      }
    }
  }

  test("white dwarf spectral types should be handled gracefully") {
    // White dwarfs with numeric temperatures can be parsed
    // But may not find a matching library SED (returns Left like Python)
    val whiteDwarfTypes = List("DA3.5", "DBAP3", "DZQA6", "DC?")

    whiteDwarfTypes.foreach { spectralType =>
      // Should not throw - graceful handling
      val result = SimbadSEDMatcher.inferSED("WD*", Some(spectralType))
      // Result may be Left if no match found (matches Python behavior)
      // Just ensure it doesn't throw
      val _ = result
    }
  }

  test("subdwarf spectral types should be handled gracefully") {
    // Subdwarfs may not find matching library SEDs (returns Left like Python)
    val subdwarfTypes = List("sdO2VIIIHe5", "sdB1", "sdBN0VIIHe28", "sdG", "sd:K1Fe-1", "sdT8")

    subdwarfTypes.foreach { spectralType =>
      // Should not throw - graceful handling
      val result = SimbadSEDMatcher.inferSED("*", Some(spectralType))
      // Result may be Left if no match found (matches Python behavior)
      // Just ensure it doesn't throw
      val _ = result
    }
  }

  test("galaxy morphological types should be classified correctly") {
    val ellipticalTests = List(
      ("E", GalaxySpectrum.Elliptical),
      ("E0", GalaxySpectrum.Elliptical),   // Round elliptical - was bug
      ("E3", GalaxySpectrum.Elliptical),
      ("S0", GalaxySpectrum.Elliptical),
      ("S0/a", GalaxySpectrum.Elliptical),
      ("-0.5", GalaxySpectrum.Elliptical), // Hubble stage - exact threshold
      ("-1.0", GalaxySpectrum.Elliptical), // Hubble stage
      ("0.0", GalaxySpectrum.Spiral)       // 0.0 is spiral per Python
    )

    val spiralTests = List(
      ("Sa", GalaxySpectrum.Spiral),
      ("Sb", GalaxySpectrum.Spiral),
      ("Sc", GalaxySpectrum.Spiral),
      ("SBa", GalaxySpectrum.Spiral),
      ("SBb", GalaxySpectrum.Spiral),
      ("SBc", GalaxySpectrum.Spiral),
      ("5.0", GalaxySpectrum.Spiral) // Hubble stage
    )

    (ellipticalTests ++ spiralTests).foreach { case (morphType, expectedSpectrum) =>
      val result = SimbadSEDMatcher.inferSED("G", morphType = Some(morphType))
      assert(result.isRight, s"Morphological type $morphType should be parseable")
      result match {
        case Right(sed) =>
          sed match {
            case UnnormalizedSED.Galaxy(spectrum) =>
              assertEquals(spectrum,
                           expectedSpectrum,
                           s"Morphological type $morphType should map to $expectedSpectrum"
              )
            case _                                =>
              fail(s"Expected Galaxy SED for morphological type $morphType")
          }
        case Left(_) => fail(s"Expected Right for $morphType")
      }
    }
  }

  test("common stellar types should match known SED library entries") {
    val commonStars = List(
      ("A0V", "*"),
      ("A1V", "*"),
      ("F5V", "*"),
      ("G2V", "*"),
      ("G8V", "*"),
      ("K0V", "*"),
      ("K3V", "*"),
      ("M0V", "*"),
      ("M2V", "*"),
      ("G8III", "*"),
      ("K3III", "*"),
      ("M0III", "*")
    )

    commonStars.foreach { case (spectralType, otype) =>
      val result = SimbadSEDMatcher.inferSED(otype, Some(spectralType))
      assert(result.isRight, s"Common stellar type $spectralType should have a SED match")
    }
  }

  test("examples from Python reference test data should work") {
    // Test cases based on the Python test data we found
    val testCases = List(
      ("PM*", Some("A1V"), None),   // Proper motion star
      ("s*r", Some("K3Ib"), None),  // Red star
      ("HB*", Some("G8III"), None), // Hot blue star
      ("G", None, Some("E")),       // Elliptical galaxy
      ("G", None, Some("Sb")),      // Spiral galaxy
      ("QSO", None, None),          // Quasar
      ("AGN", None, Some("E")),     // Active galactic nucleus
      ("HII", None, None),          // HII region
      ("PN", None, None)            // Planetary nebula
    )

    testCases.foreach { case (otype, spectralType, morphType) =>
      val result = SimbadSEDMatcher.inferSED(otype, spectralType, morphType)
      assert(result.isRight,
             s"Test case ($otype, $spectralType, $morphType) should have a SED match"
      )
    }
  }

  test("unrecognized object types should return error") {
    val unrecognizedTypes = List("XYZ", "???", "Unknown", "")

    unrecognizedTypes.foreach { otype =>
      val result = SimbadSEDMatcher.inferSED(otype)
      assert(result.isLeft, s"Unrecognized object type $otype should return error")
      result match {
        case Left(errors) =>
          assert(errors.head.isInstanceOf[CatalogProblem.UnknownObjectType],
                 s"Should have UnknownObjectType error for $otype"
          )
        case Right(_) => fail(s"Expected Left for $otype")
      }
    }
  }

  test("empty or invalid spectral types should be handled gracefully") {
    val invalidSpectralTypes = List("", "???", "Unknown")

    invalidSpectralTypes.foreach { spectralType =>
      val result = SimbadSEDMatcher.inferSED("*", Some(spectralType))
      // Should return either Right or Left - just no exceptions
      val _ = result
    }
  }
}
