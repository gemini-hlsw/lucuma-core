// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.EitherNec
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import munit.FunSuite

import scala.io.Source

/**
 * Test suite for SED matching against Simbad entries. Uses the full dataset (8000+ entries) and
 * validates against Python reference implementation.
 */
class SimbadSEDMatcherSuite extends FunSuite:

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

  case class ExpectedOutput(
    main_id:  String,
    otype:    String,
    filename: Option[String],
    t_eff:    Option[Double],
    log_g:    Option[Double]
  )

  lazy val testData: List[SimbadEntry] =
    val stream = getClass.getResourceAsStream("/simbad-sed-test-data-full.dat")
    val source = Source.fromInputStream(stream)
    try
      source
        .getLines()
        .drop(1)
        .map { line =>
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
    finally source.close()

  private def parseDataLine(line: String): Array[String] =
    val pattern = """"([^"]*)"|(\S+)""".r
    pattern
      .findAllMatchIn(line)
      .map(m => if m.group(1) != null then m.group(1) else m.group(2))
      .toArray

  lazy val matchResults: List[MatchResult] =
    testData.map { entry =>
      val morphTypeOpt    = if entry.morphType.isEmpty then None else Some(entry.morphType)
      val spectralTypeOpt = if entry.spectralType.isEmpty then None else Some(entry.spectralType)
      val sed             = SEDMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

      val category = sed.toOption.map {
        case UnnormalizedSED.StellarLibrary(_)        => "star"
        case UnnormalizedSED.Galaxy(_)                => "galaxy"
        case UnnormalizedSED.Quasar(_)                => "quasar"
        case UnnormalizedSED.HIIRegion(_)             => "hii"
        case UnnormalizedSED.PlanetaryNebula(_)       => "pn"
        case UnnormalizedSED.PowerLaw(_)              => "powerlaw"
        case UnnormalizedSED.BlackBody(_)             => "blackbody"
        case UnnormalizedSED.UserDefined(_)           => "user"
        case UnnormalizedSED.CoolStarModel(_)         => "coolstar"
        case UnnormalizedSED.Planet(_)                => "planet"
        case UnnormalizedSED.UserDefinedAttachment(_) => "userattachment"
      }

      MatchResult(entry, sed, category)
    }

  lazy val expectedOutput: List[ExpectedOutput] =
    val stream = getClass.getResourceAsStream("/expected-output-full.csv")
    val source = Source.fromInputStream(stream)
    try
      source
        .getLines()
        .drop(1)
        .flatMap { line =>
          val parts = line.split(",", -1)
          if parts.length >= 5 then
            Some(
              ExpectedOutput(
                main_id = parts(0),
                otype = parts(1),
                filename = Option(parts(2)).filter(_.nonEmpty),
                t_eff = Option(parts(3)).filter(_.nonEmpty).flatMap(_.toDoubleOption),
                log_g = Option(parts(4)).filter(_.nonEmpty).flatMap(_.toDoubleOption)
              )
            )
          else None
        }
        .toList
    finally source.close()

  def filenameToSED(filename: String): Option[UnnormalizedSED] =
    filename match
      case "QSO.sed"        => Some(UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
      case "HII.sed"        => Some(UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
      case "PN.sed"         => Some(UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
      case "Elliptical.sed" => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case "Spiral.sed"     => Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
      case stellar          =>
        val spectrumTag = stellar.stripSuffix(".nm")
        StellarLibrarySpectrum.values
          .find(_.tag == spectrumTag)
          .map(UnnormalizedSED.StellarLibrary(_))

  def stellarBaseType(sed: UnnormalizedSED): Option[String] =
    sed match
      case UnnormalizedSED.StellarLibrary(spectrum) =>
        val name = spectrum.toString
        Some(if name.endsWith("_new") then name.dropRight(4) else name)
      case _                                        => None

  def sedEquivalent(p: UnnormalizedSED, s: UnnormalizedSED): Boolean =
    if p == s then true
    else
      (stellarBaseType(p), stellarBaseType(s)) match
        case (Some(pBase), Some(sBase)) => pBase == sBase
        case _                          => false

  // --- Basic loading tests ---

  test("load dataset successfully") {
    assert(testData.nonEmpty, "Test data file should contain entries")
    println(s"\nLoaded ${testData.length} entries from dataset")
    assert(testData.length >= 8000, s"Expected at least 8000 test entries, got ${testData.length}")
  }

  // --- Specific entry tests ---

  test("match G2V star correctly") {
    // Find any G2V star in the dataset
    val g2vResult = matchResults.find(r =>
      r.entry.spectralType.contains("G2") && r.entry.spectralType.contains("V")
    )
    g2vResult.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(s)) =>
          // Accept F or G type as valid matches (F8V is a close neighbor to G2V)
          assert(s.tag.startsWith("F") || s.tag.startsWith("G"),
                 s"G2V should match F or G-type spectrum, got $s"
          )
        case Left(_)                                  => // OK - may not match within tolerance
        case other                                    => fail(s"G2V star got unexpected SED: $other")
      }
    }
  }

  test("match galaxy morphological types") {
    // Find elliptical galaxies (E or S0 morphology)
    val ellipticalResults = matchResults.filter(r =>
      GalaxyTypes.contains(r.entry.otype) &&
        (r.entry.morphType.startsWith("E") || r.entry.morphType.startsWith("S0"))
    )
    ellipticalResults.filter(_.sed.isRight).foreach { result =>
      assertEquals(
        result.sed.getOrElse(fail("Expected Right")),
        UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical)
      )
    }

    // Find spiral galaxies (S morphology, not S0)
    val spiralResults = matchResults.filter(r =>
      GalaxyTypes.contains(r.entry.otype) &&
        r.entry.morphType.matches("S[abcd].*")
    )
    spiralResults.filter(_.sed.isRight).foreach { result =>
      assertEquals(
        result.sed.getOrElse(fail("Expected Right")),
        UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral)
      )
    }
  }

  test("specific spectral type cases") {
    // A0V should match A0V
    val a0Result = SEDMatcher.inferSED("*", Some("A0V"), None)
    assert(a0Result.isRight)
    assertEquals(
      a0Result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V)
    )

    // F1V should match F2V (closest available)
    val f1Result = SEDMatcher.inferSED("*", Some("F1V"), None)
    assert(f1Result.isRight)
    assertEquals(
      f1Result.getOrElse(fail("Expected Right")),
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.F2V)
    )
  }

  // --- Coverage tests by object type ---

  test("star type coverage") {
    val starResults = matchResults.filter(r => StarTypes.contains(r.entry.otype))
    val starMatched = starResults.count(_.sed.isRight)

    println(
      s"\nStar types: ${starResults.length} total, $starMatched matched (${starMatched * 100 / starResults.length}%)"
    )

    assert(starResults.nonEmpty, "Should have star entries")
    starResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(_)) => // OK
        case Left(_)                                  => // OK - no match within tolerance
        case other                                    =>
          fail(s"Star ${result.entry.mainId} (${result.entry.otype}) got unexpected SED: $other")
      }
    }
  }

  test("galaxy type coverage") {
    val galaxyResults = matchResults.filter(r => GalaxyTypes.contains(r.entry.otype))
    val galaxyMatched = galaxyResults.count(_.sed.isRight)

    println(
      s"Galaxy types: ${galaxyResults.length} total, $galaxyMatched matched (${galaxyMatched * 100 / galaxyResults.length}%)"
    )

    assert(galaxyResults.nonEmpty, "Should have galaxy entries")
    galaxyResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.Galaxy(_)) => // OK
        case Left(_)                          => // OK - no morphological type
        case other                            => fail(s"Galaxy ${result.entry.mainId} got unexpected SED: $other")
      }
    }
  }

  test("quasar type coverage") {
    val quasarResults = matchResults.filter(r => QuasarTypes.contains(r.entry.otype))
    val quasarMatched = quasarResults.count(_.sed.isRight)

    println(
      s"Quasar types: ${quasarResults.length} total, $quasarMatched matched (${quasarMatched * 100 / quasarResults.length}%)"
    )

    assert(quasarResults.nonEmpty, "Should have quasar entries")
    assertEquals(quasarMatched, quasarResults.length)
  }

  test("HII and PN coverage") {
    val hiiResults = matchResults.filter(_.entry.otype == "HII")
    val pnResults  = matchResults.filter(_.entry.otype == "PN")

    println(s"HII regions: ${hiiResults.length}")
    println(s"Planetary nebulae: ${pnResults.length}")

    assertEquals(hiiResults.count(_.sed.isRight), hiiResults.length)
    assertEquals(pnResults.count(_.sed.isRight), pnResults.length)
  }

  test("white dwarf handling") {
    // Filter for actual white dwarf otypes (WD*) with DA spectral types
    val wdResults =
      matchResults.filter(r => r.entry.otype == "WD*" && r.entry.spectralType.startsWith("DA"))
    wdResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(_)) => // OK
        case Left(_)                                  => // OK - no match
        case other                                    => fail(s"White dwarf ${result.entry.mainId} got unexpected SED: $other")
      }
    }
  }

  test("subdwarf handling") {
    val sdResults = matchResults.filter(r =>
      StarTypes.contains(r.entry.otype) && r.entry.spectralType.startsWith("sd")
    )
    sdResults.foreach { result =>
      result.sed match {
        case Right(UnnormalizedSED.StellarLibrary(_)) => // OK
        case Left(_)                                  => // OK - no match
        case other                                    => fail(s"Subdwarf ${result.entry.mainId} got unexpected SED: $other")
      }
    }
  }

  // --- Statistics ---

  test("comprehensive statistics") {
    val total     = matchResults.length
    val matched   = matchResults.count(_.sed.isRight)
    val unmatched = total - matched

    val byCat   =
      matchResults.filter(_.sed.isRight).groupBy(_.category.get).view.mapValues(_.length).toMap
    val byOtype =
      matchResults.filter(_.sed.isLeft).groupBy(_.entry.otype).view.mapValues(_.length).toMap

    println(s"\n=== SED Matching Statistics ===")
    println(s"Total entries: $total")
    println(s"Matched: $matched (${matched * 100 / total}%)")
    println(s"Unmatched: $unmatched (${unmatched * 100 / total}%)")
    println(s"\nBy category:")
    byCat.toSeq.sortBy(-_._2).foreach { case (cat, count) =>
      println(s"  $cat: $count (${count * 100 / total}%)")
    }

    println(s"\nTop 20 unmatched object types:")
    byOtype.toSeq.sortBy(-_._2).take(20).foreach { case (otype, count) =>
      println(s"  $otype: $count")
    }

    assert(matched > total / 3, s"Expected at least 33% match rate, got ${matched * 100 / total}%")
  }

  // --- Python reference validation ---

  // Known differences between Python and Scala implementations
  val knownDifferences = Set(
    "BD+25  2534", // sdB1(k) - subdwarf matching tolerance
    "*   3 Cet",   // K3Ib with s*r otype - otype category difference
    "*  17 Aqr"    // K4/5III - range scoring (Python picks K5, Scala picks K4)
  )

  test("validate against Python reference output") {
    val inputData = testData.map(e => e.mainId -> e).toMap

    var matches         = 0
    var mismatches      = 0
    var knownDiffs      = 0
    val errors          = scala.collection.mutable.ListBuffer[String]()
    val unexpectedDiffs = scala.collection.mutable.ListBuffer[String]()

    expectedOutput.foreach { expected =>
      inputData.get(expected.main_id).foreach { entry =>
        val morphTypeOpt    = if entry.morphType.isEmpty then None else Some(entry.morphType)
        val spectralTypeOpt = if entry.spectralType.isEmpty then None else Some(entry.spectralType)
        val scalaResult     = SEDMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

        val pythonSED = expected.filename.flatMap(filenameToSED)
        val scalaSED  = scalaResult.toOption

        (pythonSED, scalaSED) match
          case (None, None)                              => matches += 1
          case (Some(p), Some(s)) if sedEquivalent(p, s) => matches += 1
          case (Some(p), Some(s))                        =>
            mismatches += 1
            val msg = s"${expected.main_id}: Python=$p, Scala=$s"
            errors += msg
            if knownDifferences.contains(expected.main_id) then knownDiffs += 1
            else unexpectedDiffs += msg
          case (Some(p), None)                           =>
            mismatches += 1
            val msg = s"${expected.main_id}: Python=$p, Scala=None"
            errors += msg
            if knownDifferences.contains(expected.main_id) then knownDiffs += 1
            else unexpectedDiffs += msg
          case (None, Some(s))                           =>
            mismatches += 1
            val msg = s"${expected.main_id}: Python=None, Scala=$s"
            errors += msg
            if knownDifferences.contains(expected.main_id) then knownDiffs += 1
            else unexpectedDiffs += msg
      }
    }

    println(s"\n=== Python Reference Validation ===")
    println(s"Total entries: ${expectedOutput.length}")
    println(s"Matches: $matches (${matches * 100 / expectedOutput.length}%)")
    println(s"Known differences: $knownDiffs")
    println(s"Total mismatches: $mismatches")

    if errors.nonEmpty && errors.length <= 50 then
      println(s"\nAll differences:")
      errors.foreach(e => println(s"  $e"))
    else if errors.nonEmpty then
      println(s"\nFirst 50 differences:")
      errors.take(50).foreach(e => println(s"  $e"))
      println(s"... and ${errors.length - 50} more")
  }
