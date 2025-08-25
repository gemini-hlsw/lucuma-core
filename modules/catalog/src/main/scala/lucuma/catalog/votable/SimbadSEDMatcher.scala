// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED

object SimbadSEDMatcher {

  /**
   * Infer an appropriate UnnormalizedSED from Simbad object type and classification information.
   * Based on the approach from match_sed.py by Andrew Stephens.
   *
   * @param otype Simbad object type (OTYPE field)
   * @param spectralType Simbad spectral type (SP_TYPE field)
   * @param morphType Simbad morphological type (MORPH_TYPE field)
   * @return Some(UnnormalizedSED) if a match can be inferred, None otherwise
   */
  def inferSED(
    otype: String,
    spectralType: Option[String] = None,
    morphType: Option[String] = None
  ): Option[UnnormalizedSED] = {

    parseObjectType(otype) match {
      case Some(ObjectCategory.Star) =>
        spectralType.flatMap(matchStellarSED)
      case Some(ObjectCategory.Galaxy) =>
        morphType.flatMap(matchGalaxySED)
      case Some(ObjectCategory.Quasar) =>
        Some(UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
      case Some(ObjectCategory.HIIRegion) =>
        Some(UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
      case Some(ObjectCategory.PlanetaryNebula) =>
        Some(UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
      case None =>
        None
    }
  }

  /**
   * Broad object categories for SED classification.
   * Maps to the categories used in the Python reference implementation.
   */
  private enum ObjectCategory:
    case Star, Galaxy, Quasar, HIIRegion, PlanetaryNebula

  /**
   * Parse Simbad OTYPE to determine broad object category.
   * Based on simbad_otype mappings from match_sed.py parse_otype function.
   */
  private def parseObjectType(otype: String): Option[ObjectCategory] = {
    val starTypes = Set(
      "*", "Ma*", "bC*", "sg*", "s*r", "s*y", "s*b", "WR*", "N*", "Psr",
      "Y*O", "Or*", "TT*", "Ae*", "HH",
      "MS*", "Be*", "Ms*", "SB*", "BS*", "SX*", "gD*", "dS*",
      "Ev*", "RG*", "HS*", "HB*", "RR*", "WV*", "Ce*", "cC*", "C*", "S*", "LP*", "AB*",
      "Mi*", "OH*", "pA*", "RV*", "WD*", "RG*",
      "Pe*", "a2*", "RC*",
      "LM*", "BD*",
      "Ir*", "Er*", "Ro*", "Pu*", "Em*",
      "PM*", "HV*",
      "**"  // Double star systems are typically stellar
    )

    val galaxyTypes = Set(
      "G", "GGG", "LSB", "bCG", "SBG", "H2G", "EmG", "rG",
      "GiP", "GiG", "GiC", "BiC", "IG", "PaG", "GrG", "CGG", "CIG", "PCG", "SCG"
    )

    val quasarTypes = Set(
      "AGN", "AG?", "SyG", "Sy1", "Sy2", "QSO", "Q?", "Bla", "Bz?", "BLL", "BL?", "LIN"
    )

    val hiiTypes = Set("HII")
    val pnTypes = Set("PN")

    if (starTypes.contains(otype)) Some(ObjectCategory.Star)
    else if (galaxyTypes.contains(otype)) Some(ObjectCategory.Galaxy)
    else if (quasarTypes.contains(otype)) Some(ObjectCategory.Quasar)
    else if (hiiTypes.contains(otype)) Some(ObjectCategory.HIIRegion)
    else if (pnTypes.contains(otype)) Some(ObjectCategory.PlanetaryNebula)
    else None
  }

  /**
   * Match a stellar spectral type to an appropriate StellarLibrarySpectrum.
   * Based on the spectral type parsing from match_sed.py.
   */
  private def matchStellarSED(spectralType: String): Option[UnnormalizedSED] = {
    parseSpectralType(spectralType) match {
      case Some((luminosityClasses, temperatureClasses)) =>
        findBestStellarMatch(luminosityClasses, temperatureClasses).map { spectrum =>
          UnnormalizedSED.StellarLibrary(spectrum)
        }
      case None => None
    }
  }

  /**
   * Data class to hold parsed spectral type components.
   */
  private case class SpectralComponents(
    luminosityClasses: List[String],
    temperatureClasses: List[String]
  )

  /**
   * Parse Simbad spectral type string into luminosity and temperature classes.
   * Based on parse_sp_type function from match_sed.py.
   */
  private def parseSpectralType(spectralType: String): Option[(List[String], List[String])] = {
    if (spectralType.isEmpty) return None

    // Clean up the spectral type string
    val cleaned = spectralType.replaceAll("[():]", "")

    // Handle white dwarfs (D prefix)
    if (cleaned.startsWith("D")) {
      val dwPattern = """^D[ABCGKMOQPXZ]*""".r
      dwPattern.findFirstIn(cleaned) match {
        case Some(dwMatch) =>
          val lClass = dwMatch
          val tClass = cleaned.substring(dwMatch.length)
          return Some((List(lClass), List(tClass)))
        case None =>
          return Some((List("D"), List(cleaned.substring(1))))
      }
    }

    // Handle subdwarfs (sd prefix)
    if (cleaned.startsWith("sd")) {
      val tempPattern = """[OBAFGKMLTY][0-9]?\.?[0-9]?[+-]?""".r
      tempPattern.findFirstIn(cleaned) match {
        case Some(tClass) => return Some((List("sd"), List(tClass)))
        case None => return Some((List("sd"), List(cleaned.substring(2))))
      }
    }

    // Handle main sequence, giants, supergiants
    val tempPattern = """^[OBAFGKMLTY][0-9]?(\.5)?[+-]?[-/]?[OBAFGKMLTY]?[0-9]?(\.5)?[+-]?""".r
    val tempClasses = tempPattern.findFirstIn(cleaned) match {
      case Some(tMatch) =>
        // Split temperature ranges like "G8/K0" or "M8-9"
        val rangePattern = """^[OBAFGKMLTY][0-9]?(\.5)?[+-]?-[OBAFGKMLTY]?[0-9](\.5)?[+-]?""".r
        if (rangePattern.matches(tMatch)) {
          tMatch.split("-").toList
        } else {
          tMatch.split("/").toList
        }
      case None => List.empty
    }

    // Normalize temperature classes (e.g., convert "8" to "M8" if first is "M8")
    val normalizedTempClasses = if (tempClasses.length > 1 && tempClasses.head.nonEmpty) {
      val firstLetter = tempClasses.head.charAt(0)
      tempClasses.map { tc =>
        if (tc.matches("[0-9].*") && !tc.contains(firstLetter)) {
          s"$firstLetter$tc"
        } else tc
      }
    } else tempClasses

    // Extract luminosity classes
    val lumPattern = """[IV]+[ab]*[-/]*[IV]*[ab]*""".r
    val lumClasses = lumPattern.findFirstIn(cleaned) match {
      case Some(lMatch) => lMatch.split("[-/]").toList
      case None => List.empty
    }

    // Normalize luminosity classes
    val normalizedLumClasses = if (lumClasses.length > 1 && lumClasses.head.nonEmpty) {
      val firstRoman = lumClasses.head.takeWhile("IV".contains(_))
      lumClasses.map { lc =>
        if (lc.matches("[ab]+") && firstRoman.nonEmpty) {
          s"$firstRoman$lc"
        } else lc
      }
    } else lumClasses

    if (normalizedTempClasses.nonEmpty || normalizedLumClasses.nonEmpty) {
      Some((normalizedLumClasses, normalizedTempClasses))
    } else {
      None
    }
  }

  /**
   * Find the best matching StellarLibrarySpectrum for given spectral classes.
   */
  private def findBestStellarMatch(
    luminosityClasses: List[String],
    temperatureClasses: List[String]
  ): Option[StellarLibrarySpectrum] = {

    // If no temperature classes, return a default
    if (temperatureClasses.isEmpty && luminosityClasses.isEmpty) {
      return Some(StellarLibrarySpectrum.G2V)
    }

    val candidates = StellarLibrarySpectrum.values.toList
    val tempClass = temperatureClasses.headOption.getOrElse("")
    val lumClass = luminosityClasses.headOption.getOrElse("V") // Default to main sequence

    // For white dwarfs and subdwarfs, use a generic stellar spectrum
    if (lumClass.startsWith("D") || lumClass == "sd") {
      return Some(StellarLibrarySpectrum.G2V) // Use solar-type as default
    }

    // Try exact match first
    val exactMatch = candidates.find { spectrum =>
      val tag = spectrum.tag
      tag.contains(tempClass) && tag.contains(lumClass)
    }

    if (exactMatch.isDefined) return exactMatch

    // Try temperature class match with default luminosity
    val tempMatch = candidates.find { spectrum =>
      spectrum.tag.contains(tempClass) && spectrum.tag.contains("V")
    }

    if (tempMatch.isDefined) return tempMatch

    // Try to match just the spectral class letter
    val tempLetter = tempClass.take(1)
    val letterMatch = candidates.find { spectrum =>
      spectrum.tag.startsWith(tempLetter) && spectrum.tag.contains(lumClass)
    }

    letterMatch.orElse {
      // Last resort: match just the letter with main sequence
      candidates.find { spectrum =>
        spectrum.tag.startsWith(tempLetter) && spectrum.tag.contains("V")
      }
    }.orElse {
      // Final fallback: return solar type
      Some(StellarLibrarySpectrum.G2V)
    }
  }

  /**
   * Match galaxy morphological type to appropriate GalaxySpectrum.
   * Based on match_galaxy function from match_sed.py.
   */
  private def matchGalaxySED(morphType: String): Option[UnnormalizedSED] = {
    val ellipticalPattern = """E[1-9:]?""".r
    val s0Pattern = """S0.*""".r
    val spiralPattern = """S[abcABC_:]?.*""".r
    val hubbleStagePattern = """-?[0-9]+\.?[0-9]*""".r

    morphType match {
      case ellipticalPattern() =>
        Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case s0Pattern() =>
        Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case spiralPattern() =>
        Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
      case hubbleStagePattern() =>
        // Numerical Hubble stage classification
        val hubbleStage = morphType.toDoubleOption.getOrElse(0.0)
        if (hubbleStage < 0.0) {
          Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
        } else if (hubbleStage < 9) {
          Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
        } else {
          None
        }
      case _ => None
    }
  }
}
