// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import lucuma.catalog.votable.SimbadSEDMatcher
import lucuma.core.model.UnnormalizedSED
import munit.FunSuite

import scala.io.Source

/**
 * Comprehensive test suite using the full match_sed_test.dat dataset (8000+ entries). Tests SED
 * matching against the complete Python reference dataset.
 */
class SimbadSEDMatcherFullDataSuite extends FunSuite:

  case class SimbadEntry(
    mainId:       String,
    otype:        String,
    morphType:    String,
    spectralType: String
  )

  case class MatchResult(
    entry:    SimbadEntry,
    sed:      Option[UnnormalizedSED],
    category: Option[String]
  )

  // Load test data from resource file
  lazy val testData: List[SimbadEntry] =
    val stream = getClass.getResourceAsStream("/simbad-sed-test-data-full.dat")
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
   * Parse a data line handling quoted fields.
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
      val sed             = SimbadSEDMatcher.inferSED(entry.otype, spectralTypeOpt, morphTypeOpt)

      val category = sed.map {
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

  test("load full dataset successfully") {
    assert(testData.nonEmpty, "Test data file should contain entries")
    println(s"\nLoaded ${testData.length} entries from full dataset")
    assert(testData.length >= 8000, s"Expected at least 8000 test entries, got ${testData.length}")
  }

  test("comprehensive statistics") {
    val total     = matchResults.length
    val matched   = matchResults.count(_.sed.isDefined)
    val unmatched = total - matched

    val byCat   =
      matchResults.filter(_.sed.isDefined).groupBy(_.category.get).view.mapValues(_.length).toMap
    val byOtype =
      matchResults.filter(_.sed.isEmpty).groupBy(_.entry.otype).view.mapValues(_.length).toMap

    println(s"\n=== Full Dataset SED Matching Results ===")
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
    println()

    // Basic sanity checks - without G2V fallback, match rate is lower but more accurate
    // 47% is expected when only returning matches within physics tolerance
    assert(matched > total / 3, s"Expected at least 33% match rate, got ${matched * 100 / total}%")
  }

  test("star type coverage") {
    val starResults = matchResults.filter(r => StarTypes.contains(r.entry.otype))
    val starMatched = starResults.count(_.sed.isDefined)

    println(
      s"\nStar types: ${starResults.length} total, $starMatched matched (${starMatched * 100 / starResults.length}%)"
    )

    assert(starResults.nonEmpty, "Should have star entries")
  }

  test("galaxy type coverage") {
    val galaxyResults = matchResults.filter(r => GalaxyTypes.contains(r.entry.otype))
    val galaxyMatched = galaxyResults.count(_.sed.isDefined)

    println(
      s"Galaxy types: ${galaxyResults.length} total, $galaxyMatched matched (${galaxyMatched * 100 / galaxyResults.length}%)"
    )

    assert(galaxyResults.nonEmpty, "Should have galaxy entries")
  }

  test("quasar type coverage") {
    val quasarResults = matchResults.filter(r => QuasarTypes.contains(r.entry.otype))
    val quasarMatched = quasarResults.count(_.sed.isDefined)

    println(
      s"Quasar types: ${quasarResults.length} total, $quasarMatched matched (${quasarMatched * 100 / quasarResults.length}%)"
    )

    assert(quasarResults.nonEmpty, "Should have quasar entries")
    // All recognized quasar types should match
    assertEquals(quasarMatched, quasarResults.length)
  }

  test("HII and PN coverage") {
    val hiiResults = matchResults.filter(_.entry.otype == "HII")
    val pnResults  = matchResults.filter(_.entry.otype == "PN")

    println(s"HII regions: ${hiiResults.length}")
    println(s"Planetary nebulae: ${pnResults.length}")

    // All HII and PN should match
    assertEquals(hiiResults.count(_.sed.isDefined), hiiResults.length)
    assertEquals(pnResults.count(_.sed.isDefined), pnResults.length)
  }

  test("spectral type parsing coverage") {
    val withSpectralType   = matchResults.filter(_.entry.spectralType.nonEmpty)
    val spectralTypeParsed = withSpectralType.filter { r =>
      StarTypes.contains(r.entry.otype) && r.sed.isDefined
    }

    println(s"\nEntries with spectral type: ${withSpectralType.length}")
    println(
      s"Successfully parsed and matched: ${spectralTypeParsed.length} (${spectralTypeParsed.length * 100 / withSpectralType.length}%)"
    )
  }

  test("morphological type parsing coverage") {
    val withMorphType   = matchResults.filter(_.entry.morphType.nonEmpty)
    val morphTypeParsed = withMorphType.filter { r =>
      GalaxyTypes.contains(r.entry.otype) && r.sed.isDefined
    }

    println(s"\nEntries with morphological type: ${withMorphType.length}")
    println(s"Successfully parsed and matched: ${morphTypeParsed.length}")
  }
