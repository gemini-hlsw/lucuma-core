// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.option.*
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED

/**
 * Broad object categories for SED classification. Maps to the categories used in the Python
 * implementation.
 */
private enum ObjectCategory:
  case Star, Galaxy, Quasar, HIIRegion, PlanetaryNebula

object SimbadSEDMatcher:
  /**
   * Infer an appropriate UnnormalizedSED from Simbad object type and classification information.
   * Based on match_sed.py by Andrew Stephens.
   *
   * @param otype
   *   Simbad object type (OTYPE field)
   * @param spectralType
   *   Simbad spectral type (SP_TYPE field)
   * @param morphType
   *   Simbad morphological type (MORPH_TYPE field)
   */
  def inferSED(
    otype:        String,
    spectralType: Option[String] = None,
    morphType:    Option[String] = None
  ): Option[UnnormalizedSED] =
    parseObjectType(otype) match {
      case Some(ObjectCategory.Star)            =>
        spectralType.flatMap(matchStellarSED)
      case Some(ObjectCategory.Galaxy)          =>
        morphType.flatMap(matchGalaxySED)
      case Some(ObjectCategory.Quasar)          =>
        UnnormalizedSED.Quasar(QuasarSpectrum.QS0).some
      case Some(ObjectCategory.HIIRegion)       =>
        UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula).some
      case Some(ObjectCategory.PlanetaryNebula) =>
        UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009).some
      case None                                 =>
        none
    }

  /**
   * Parse Simbad OTYPE to determine broad object category. Based on simbad_otype mappings from
   * match_sed.py parse_otype function.
   */
  private def parseObjectType(otype: String): Option[ObjectCategory] =

    if (StarTypes.contains(otype)) Some(ObjectCategory.Star)
    else if (GalaxyTypes.contains(otype)) Some(ObjectCategory.Galaxy)
    else if (QuasarTypes.contains(otype)) Some(ObjectCategory.Quasar)
    else if (HIITypes.contains(otype)) Some(ObjectCategory.HIIRegion)
    else if (PNTypes.contains(otype)) Some(ObjectCategory.PlanetaryNebula)
    else None

  /**
   * Match a stellar spectral type to an appropriate StellarLibrarySpectrum. Based on the spectral
   * type parsing from match_sed.py.
   */
  private def matchStellarSED(spectralType: String): Option[UnnormalizedSED] =
    parseSpectralType(spectralType) match {
      case Some((luminosityClasses, temperatureClasses)) =>
        findBestStellarMatch(luminosityClasses, temperatureClasses).map { spectrum =>
          UnnormalizedSED.StellarLibrary(spectrum)
        }
      case None                                          => None
    }

  /**
   * Parse Simbad spectral type string into luminosity and temperature classes. Uses parser
   * combinators for robust parsing.
   */
  private def parseSpectralType(spectralType: String): Option[(List[String], List[String])] =
    if (spectralType.isEmpty) None
    else
      // Clean up the spectral type string (remove parentheses and colons)
      val cleaned = spectralType.replaceAll("[():]", "")
      // Use parse instead of parseAll to allow trailing characters
      SpectralTypeParsers.spectralType.parse(cleaned).toOption.map(_._2)

  /**
   * Find the best matching StellarLibrarySpectrum for given spectral classes.
   */
  private def findBestStellarMatch(
    luminosityClasses:  List[String],
    temperatureClasses: List[String]
  ): Option[StellarLibrarySpectrum] =

    // Handle edge cases upfront
    if temperatureClasses.isEmpty && luminosityClasses.isEmpty then Some(StellarLibrarySpectrum.G2V)
    else
      val candidates = StellarLibrarySpectrum.values.toList
      val tempClass  = temperatureClasses.headOption.getOrElse("")
      val lumClass   = luminosityClasses.headOption.getOrElse("V") // Default to main sequence

      // White dwarfs and subdwarfs use generic stellar spectrum
      if lumClass.startsWith("D") || lumClass == "sd" then Some(StellarLibrarySpectrum.G2V)
      else
        val tempLetter = tempClass.take(1)

        // Try exact match first
        val exactMatch = candidates.find { spectrum =>
          val tag = spectrum.tag
          tag.contains(tempClass) && tag.contains(lumClass)
        }

        // Chain fallbacks using orElse
        exactMatch
          .orElse {
            // Try temperature class match with default luminosity
            candidates.find(spectrum =>
              spectrum.tag.contains(tempClass) && spectrum.tag.contains("V")
            )
          }
          .orElse {
            // Try to match just the spectral class letter
            candidates.find { spectrum =>
              spectrum.tag.startsWith(tempLetter) && spectrum.tag.contains(lumClass)
            }
          }
          .orElse {
            // Last resort: match just the letter with main sequence
            candidates.find { spectrum =>
              spectrum.tag.startsWith(tempLetter) && spectrum.tag.contains("V")
            }
          }
          .orElse {
            // Final fallback: return solar type
            Some(StellarLibrarySpectrum.G2V)
          }

  /**
   * Match galaxy morphological type to appropriate GalaxySpectrum.
   */
  private def matchGalaxySED(morphType: String): Option[UnnormalizedSED] = {
    val ellipticalPattern  = """E[1-9:]?""".r
    val s0Pattern          = """S0.*""".r
    val spiralPattern      = """S[abcABC_:]?.*""".r
    val hubbleStagePattern = """-?[0-9]+\.?[0-9]*""".r

    morphType match {
      case ellipticalPattern()  =>
        Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case s0Pattern()          =>
        Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case spiralPattern()      =>
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
      case _                    => None
    }
  }

val GalaxyTypes = Set(
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

val QuasarTypes = Set(
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

val HIITypes = Set("HII")
val PNTypes  = Set("PN")

val StarTypes = Set(
  "*",
  "Ma*",
  "bC*",
  "sg*",
  "s*r",
  "s*y",
  "s*b",
  "WR*",
  "N*",
  "Psr",
  "Y*O",
  "Or*",
  "TT*",
  "Ae*",
  "HH",
  "MS*",
  "Be*",
  "Ms*",
  "SB*",
  "BS*",
  "SX*",
  "gD*",
  "dS*",
  "Ev*",
  "RG*",
  "HS*",
  "HB*",
  "RR*",
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
  "WD*",
  "RG*",
  "Pe*",
  "a2*",
  "RC*",
  "LM*",
  "BD*",
  "Ir*",
  "Er*",
  "Ro*",
  "Pu*",
  "Em*",
  "PM*",
  "HV*"
)
