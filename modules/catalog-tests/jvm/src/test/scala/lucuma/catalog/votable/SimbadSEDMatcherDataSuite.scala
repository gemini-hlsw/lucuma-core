// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import lucuma.catalog.votable.SEDMatcher
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import munit.FunSuite

import scala.io.Source
import cats.data.EitherNec

/**
 * Test suite that verifies SED matching against a dataset of Simbad entries. Replicates the
 * functionality of match_sed_test.py from the Python reference implementation.
 */
class SimbadSEDMatcherDataSuite extends FunSuite:

  case class SimbadEntry(
    mainId:       String,
    otype:        String,
    morphType:    String,
    spectralType: String
  )

  case class MatchResult(
    entry:    SimbadEntry,
    sed:      EitherNec[CatalogProblem, UnnormalizedSED],
    category: Option[String]
  )

  // Load test data from resource file
  lazy val testData: List[SimbadEntry] =
    val stream = getClass.getResourceAsStream("/simbad-sed-test-data.dat")
    val source = Source.fromInputStream(stream)
    try
      source
        .getLines()
        .drop(1) // Skip header line
        .map { line =>
          // Parse space-separated format with quoted fields
          val parts = parseDataLine(line)
          if parts.length >= 4 then
            val mainId       = parts(0).replace("\"", "")
            val otype        = parts(1)
            val morphType    = parts(2).replace("\"", "")
            val spectralType = parts(3).replace("\"", "")
            Some(SimbadEntry(mainId, otype, morphType, spectralType))
          else None
        }
        .collect { case Some(entry) => entry }
        .toList
    finally
      source.close()

  /**
   * Parse a data line handling quoted fields. Simple parser for format: "field1" field2 "field3"
   * "field4"
   */
  private def parseDataLine(line: String): Array[String] =
    val pattern = """"([^"]*)"|(\S+)""".r
    pattern
      .findAllMatchIn(line)
      .map { m =>
        if m.group(1) != null then m.group(1) else m.group(2)
      }
      .toArray

  // Run SED matcher on all test data
  lazy val matchResults: List[MatchResult] =
    testData.map { entry =>
      val morphTypeOpt    = if entry.morphType.isEmpty then None else Some(entry.morphType)
      val spectralTypeOpt = if entry.spectralType.isEmpty then None else Some(entry.spectralType)
      val sed             = SEDMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

      val category = sed.toOption.map {
        case UnnormalizedSED.StellarLibrary(_)        => "star"
        case UnnormalizedSED.Galaxy(_)                => "galaxy"
        case UnnormalizedSED.CoolStarModel(_)         => "coolstarmodel"
        case UnnormalizedSED.Planet(_)                => "planet"
        case UnnormalizedSED.Quasar(_)                => "quasar"
        case UnnormalizedSED.HIIRegion(_)             => "hii"
        case UnnormalizedSED.PlanetaryNebula(_)       => "pn"
        case UnnormalizedSED.PowerLaw(_)              => "powerlaw"
        case UnnormalizedSED.BlackBody(_)             => "blackbody"
        case UnnormalizedSED.UserDefined(_)           => "user"
        case UnnormalizedSED.UserDefinedAttachment(_) => "user"
      }

      MatchResult(entry, sed, category)
    }

  test("load test data successfully") {
    assert(testData.nonEmpty, "Test data file should contain entries")
    assert(testData.length >= 60, s"Expected at least 60 test entries, got ${testData.length}")
  }

  test("parse test data correctly") {
    val vega = testData.find(_.mainId == "Vega")
    assert(vega.isDefined, "Vega should be in test data")
    assertEquals(vega.get.otype, "PulsV*delSct")
    assertEquals(vega.get.spectralType, "A0Va")
  }

  test("identify star object types") {
    val starResults = matchResults.filter(r =>
      Set("*",
          "PM*",
          "SB*",
          "s*r",
          "LP*",
          "**",
          "BY*",
          "HV*",
          "HS*",
          "WD*",
          "Be*",
          "Y*O",
          "delSctV*"
      ).contains(r.entry.otype)
    )
    // These should either get a stellar SED or Left (if otype not recognized)
    starResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(_)) => // OK
        case Left(_)                                  => // OK - otype not recognized (e.g., PulsV*delSct, delSctV*)
        case other                                    =>
          fail(s"Star ${result.entry.mainId} (${result.entry.otype}) got unexpected SED: $other")
      }
    }
  }

  test("identify galaxy object types") {
    // Filter for galaxy types (not including SyG/Sy2 which are Quasar types per Python)
    val galaxyResults = matchResults.filter(r => GalaxyTypes.contains(r.entry.otype))
    assert(galaxyResults.nonEmpty, "Should have galaxy entries in test data")
    galaxyResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.Galaxy(_)) => // OK
        case Left(_)                           => // OK - no morphological type or doesn't match
        case other                             => fail(s"Galaxy ${result.entry.mainId} got unexpected SED: $other")
      }
    }
  }

  test("identify quasar object types") {
    val quasarResults = matchResults.filter(r => Set("QSO", "BLL").contains(r.entry.otype))
    assert(quasarResults.nonEmpty, "Should have quasar entries in test data")
    quasarResults.foreach { result =>
      assert(result.sed.isRight)
      assertEquals(result.sed.getOrElse(fail("Expected Right")), UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
    }
  }

  test("identify HII region object types") {
    val hiiResults = matchResults.filter(r => r.entry.otype == "HII")
    assert(hiiResults.nonEmpty, "Should have HII region entries in test data")
    hiiResults.foreach { result =>
      assert(result.sed.isRight)
      assertEquals(result.sed.getOrElse(fail("Expected Right")), UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
    }
  }

  test("identify planetary nebula object types") {
    val pnResults = matchResults.filter(r => r.entry.otype == "PN")
    assert(pnResults.nonEmpty, "Should have PN entries in test data")
    pnResults.foreach { result =>
      assert(result.sed.isRight)
      assertEquals(result.sed.getOrElse(fail("Expected Right")),
                   UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009)
      )
    }
  }

  test("match stellar spectral types correctly") {
    val sunResult = matchResults.find(_.entry.mainId == "Sun")
    assert(sunResult.isDefined)
    assert(sunResult.get.sed.isRight)
    assertEquals(sunResult.get.sed.getOrElse(fail("Expected Right")),
                 UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.G2V)
    )
  }

  test("handle white dwarf spectral types") {
    val wdResults = matchResults.filter(_.entry.spectralType.startsWith("DA"))
    // White dwarfs may not find a matching library SED (returns Left like Python)
    wdResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(_)) => // OK - found a match
        case Left(_)                                  => // OK - no match found (matches Python behavior)
        case other                                    => fail(s"White dwarf ${result.entry.mainId} got unexpected SED: $other")
      }
    }
  }

  test("handle subdwarf spectral types") {
    // Filter for star types with subdwarf spectral classification
    val sdResults = matchResults.filter(r =>
      StarTypes.contains(r.entry.otype) && r.entry.spectralType.startsWith("sd")
    )
    // Subdwarfs may not find a matching library SED (returns Left like Python)
    sdResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(_)) => // OK - found a match
        case Left(_)                                  => // OK - no match found (matches Python behavior)
        case other                                    => fail(s"Subdwarf ${result.entry.mainId} got unexpected SED: $other")
      }
    }
  }

  test("match galaxy morphological types") {
    // Elliptical galaxy
    val m87 = matchResults.find(_.entry.mainId == "M  87")
    assert(m87.isDefined)
    assert(m87.get.sed.isRight)
    assertEquals(m87.get.sed.getOrElse(fail("Expected Right")), UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))

    // Spiral galaxy
    val m31 = matchResults.find(_.entry.mainId == "M  31")
    assert(m31.isDefined)
    assert(m31.get.sed.isRight)
    assertEquals(m31.get.sed.getOrElse(fail("Expected Right")), UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
  }

  test("print summary statistics") {
    val total     = matchResults.length
    val matched   = matchResults.count(_.sed.isRight)
    val unmatched = total - matched

    val byCat =
      matchResults.filter(_.sed.isRight).groupBy(_.category.get).view.mapValues(_.length).toMap

    println(s"\n=== SED Matching Results ===")
    println(s"Total entries: $total")
    println(s"Matched: $matched (${matched * 100 / total}%)")
    println(s"Unmatched: $unmatched (${unmatched * 100 / total}%)")
    println(s"\nBy category:")
    byCat.toSeq.sortBy(_._1).foreach { case (cat, count) =>
      println(s"  $cat: $count")
    }
    println()

    // Print some examples of unmatched entries
    val unmatchedEntries = matchResults.filter(_.sed.isLeft).take(10)
    if unmatchedEntries.nonEmpty then
      println("Sample unmatched entries:")
      unmatchedEntries.foreach { result =>
        println(
          s"  ${result.entry.mainId}: otype=${result.entry.otype}, sp_type=${result.entry.spectralType}"
        )
      }
      println()
  }

  test("compare specific cases with Python reference") {
    // These are test cases matching Python behavior

    // O9.7IIn - luminosity class II may not have close match in library
    // Returns Left if outside tolerance (matches Python behavior)
    val o9Result = SEDMatcher.inferSED("*", Some("O9.7IIn"), None)
    assert(o9Result.isLeft || o9Result.exists(_.isInstanceOf[UnnormalizedSED.StellarLibrary]),
           s"O9.7IIn should return Left or StellarLibrary, got $o9Result"
    )

    // A0V should match A0V
    val a0Result = SEDMatcher.inferSED("*", Some("A0V"), None)
    assert(a0Result.isRight)
    assertEquals(a0Result.getOrElse(fail("Expected Right")), UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V))

    // F1V should match F2V (closest available)
    val f1Result = SEDMatcher.inferSED("*", Some("F1V"), None)
    assert(f1Result.isRight)
    assertEquals(f1Result.getOrElse(fail("Expected Right")), UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.F2V))
  }
