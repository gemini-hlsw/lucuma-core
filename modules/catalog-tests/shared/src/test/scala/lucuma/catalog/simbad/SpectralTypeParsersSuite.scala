// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.syntax.either.*
import cats.syntax.option.*
import munit.FunSuite

class SpectralTypeParsersSuite extends FunSuite:

  test("tempClass parser"):
    assertEquals(SpectralTypeParsers.tempClass.parseAll("G2"), "G2".asRight)
    assertEquals(SpectralTypeParsers.tempClass.parseAll("K5"), "K5".asRight)
    assertEquals(SpectralTypeParsers.tempClass.parseAll("M0"), "M0".asRight)
    assertEquals(SpectralTypeParsers.tempClass.parseAll("G3.5"), "G3.5".asRight)
    assertEquals(SpectralTypeParsers.tempClass.parseAll("K2.5"), "K2.5".asRight)
    assertEquals(SpectralTypeParsers.tempClass.parseAll("K6+"), "K6+".asRight)
    assertEquals(SpectralTypeParsers.tempClass.parseAll("K6-"), "K6-".asRight)

  test("lumClass parser"):
    assertEquals(SpectralTypeParsers.lumClass.parseAll("V"), "V".asRight)
    assertEquals(SpectralTypeParsers.lumClass.parseAll("III"), "III".asRight)
    assertEquals(SpectralTypeParsers.lumClass.parseAll("I"), "I".asRight)
    assertEquals(SpectralTypeParsers.lumClass.parseAll("IV"), "IV".asRight)
    assertEquals(SpectralTypeParsers.lumClass.parseAll("Ia"), "Ia".asRight)
    assertEquals(SpectralTypeParsers.lumClass.parseAll("Ib"), "Ib".asRight)
    assertEquals(SpectralTypeParsers.lumClass.parseAll("IIIb"), "IIIb".asRight)

  test("tempRange parser"):
    assertEquals(SpectralTypeParsers.tempRange.parseAll("G2"), List("G2").asRight)
    assertEquals(SpectralTypeParsers.tempRange.parseAll("G8/K0"), List("G8", "K0").asRight)
    assertEquals(SpectralTypeParsers.tempRange.parseAll("M2/3"), List("M2", "M3").asRight)

  test("lumRange parser"):
    assertEquals(SpectralTypeParsers.lumRange.parseAll("V"), List("V").asRight)
    assertEquals(SpectralTypeParsers.lumRange.parseAll("IV/V"), List("IV", "V").asRight)

  // White dwarf tests
  test("spectralType parses white dwarfs"):
    assertEquals(SpectralTypeParsers.spectralType.parseAll("DA3"), (List("DA"), List("3")).asRight)
    assertEquals(SpectralTypeParsers.spectralType.parseAll("DB4"), (List("DB"), List("4")).asRight)
    assertEquals(SpectralTypeParsers.spectralType.parseAll("DC"), (List("DC"), List.empty).asRight)
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("DA3.5"),
      (List("DA"), List("3.5")).asRight
    )
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("DA.8"),
      (List("DA"), List(".8")).asRight
    )
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("DBAP3"),
      (List("DBAP"), List("3")).asRight
    )

  test("spectralType parses subdwarfs"):
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("sdO2"),
      (List("sd"), List("O2")).asRight
    )
    assertEquals(SpectralTypeParsers.spectralType.parseAll("sdG"), (List("sd"), List("G")).asRight)
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("sdB1"),
      (List("sd"), List("B1")).asRight
    )

  test("spectralType parses main sequence stars"):
    assertEquals(SpectralTypeParsers.spectralType.parseAll("G2V"), (List("V"), List("G2")).asRight)
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("K3III"),
      (List("III"), List("K3")).asRight
    )
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("A0Ia"),
      (List("Ia"), List("A0")).asRight
    )
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("G8/K0III"),
      (List("III"), List("G8", "K0")).asRight
    )

  test("spectralType parses temperature only"):
    assertEquals(SpectralTypeParsers.spectralType.parseAll("G2"), (List.empty, List("G2")).asRight)
    assertEquals(SpectralTypeParsers.spectralType.parseAll("M5"), (List.empty, List("M5")).asRight)
    assertEquals(
      SpectralTypeParsers.spectralType.parseAll("A0mA1Va"),
      (List("Va"), List("A0")).asRight
    )

  test("tempClass with trailing minus modifier"):
    assertEquals(SpectralTypeParsers.tempClass.parseAll("K3-"), "K3-".asRight)

  test("tempClass with trailing plus modifier"):
    assertEquals(SpectralTypeParsers.tempClass.parseAll("G5+"), "G5+".asRight)

  test("spectralType handles range like M3-5 (not a modifier)"):
    assert(SpectralTypeParsers.spectralType.parseAll("M3-5III").isRight)

  test("spectralType handles nebular emission with non-V class"):
    val result = SpectralTypeParsers.spectralType.parseAll("O9IIn")
    assertEquals(result.getOrElse(fail("Expected Right"))._1.headOption, "II".some)
