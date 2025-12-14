// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.syntax.all.*
import lucuma.catalog.votable.CatalogProblem.*
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED

/**
 * Broad object categories for SED classification. Maps to the categories used in the Python
 * implementation.
 */
private enum ObjectCategory:
  case Star, Galaxy, Quasar, HIIRegion, PlanetaryNebula

object SimbadSEDMatcher:
  // Stellar matching tolerances (physics-based scoring)
  private val TemperatureToleranceFraction: Double = 0.1   // 10% of target temperature
  private val GravityToleranceDex: Double = 0.5             // 0.5 dex in log(g)

  // Galaxy Hubble stage classification boundaries
  private val EllipticalHubbleStageThreshold: Double = -0.5 // E0-S0
  private val SpiralHubbleStageThreshold: Double = 9.0      // Sa-Sm

  // Galaxy morphological type patterns
  private val EllipticalPattern: String = """E[0-9:+]?.*"""
  private val S0Pattern: String = """S0.*"""
  private val SpiralPattern: String = """S[abcABC_:]?.*"""
  /**
   * Infer an appropriate UnnormalizedSED from Simbad object classification.
   *
   * Uses physics-based matching for stars (effective temperature and surface gravity) and
   * morphological patterns for galaxies. Quasars, HII regions, and planetary nebulae are assigned
   * fixed spectra.
   *
   * For stars, matching is performed by comparing target stellar parameters (calculated from
   * spectral type) against all library SEDs. The match must satisfy both temperature (±10%)
   * and gravity (±0.5 dex) tolerances.
   *
   * Based on the reference implementation by Andrew Stephens (match_sed.py).
   *
   * @param otype
   *   Simbad object type code (e.g., "*", "G", "QSO"). See [[StarTypes]], [[GalaxyTypes]],
   *   [[QuasarTypes]], [[HIITypes]], [[PNTypes]].
   * @param spectralType
   *   Optional spectral classification string (e.g., "G2V", "K3III", "DA3"). Required for
   *   stellar objects to perform physics-based matching.
   * @param morphType
   *   Optional morphological type for galaxies (e.g., "Sa", "E3", or Hubble stage like "2.0").
   * @return
   *   Either a [[NonEmptyChain]] of [[CatalogProblem]] errors if matching fails, or a matched
   *   [[UnnormalizedSED]] if successful.
   *
   * @example
   * {{{
   * inferSED("*", Some("G2V"))        // Right(UnnormalizedSED.StellarLibrary(...))
   * inferSED("G", None, Some("Sa"))   // Right(UnnormalizedSED.Galaxy(...))
   * inferSED("QSO", None, None)       // Right(UnnormalizedSED.Quasar(...))
   * inferSED("???", None, None)       // Left(UnknownObjectType("???"))
   * }}}
   *
   * @see
   *   [[https://simbad.u-strasbg.fr/Pages/guide/otypes.htx Simbad Object Types]]
   * @see
   *   [[https://simbad.u-strasbg.fr/Pages/guide/chD.htx Simbad Spectral Types]]
   */
  def inferSED(
    otype:        String,
    spectralType: Option[String] = None,
    morphType:    Option[String] = None
  ): EitherNec[CatalogProblem, UnnormalizedSED] =
    parseObjectType(otype) match {
      case Some(ObjectCategory.Star)            =>
        spectralType
          .toRight(NonEmptyChain.one(InvalidSpectralType("")))
          .flatMap(matchStellarSED)
      case Some(ObjectCategory.Galaxy)          =>
        morphType
          .toRight(NonEmptyChain.one(InvalidMorphologicalType("")))
          .flatMap(matchGalaxySED)
      case Some(ObjectCategory.Quasar)          =>
        Right(UnnormalizedSED.Quasar(QuasarSpectrum.QS0))
      case Some(ObjectCategory.HIIRegion)       =>
        Right(UnnormalizedSED.HIIRegion(HIIRegionSpectrum.OrionNebula))
      case Some(ObjectCategory.PlanetaryNebula) =>
        Right(UnnormalizedSED.PlanetaryNebula(PlanetaryNebulaSpectrum.NGC7009))
      case None                                 =>
        Left(NonEmptyChain.one(UnknownObjectType(otype)))
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
  private def matchStellarSED(spectralType: String): EitherNec[CatalogProblem, UnnormalizedSED] =
    parseSpectralType(spectralType) match {
      case Some((luminosityClasses, temperatureClasses)) =>
        findBestStellarMatch(luminosityClasses, temperatureClasses)
          .map(spectrum => UnnormalizedSED.StellarLibrary(spectrum))
          .toRight(NonEmptyChain.one(UnmatchedSpectralType(spectralType)))
      case None =>
        Left(NonEmptyChain.one(InvalidSpectralType(spectralType)))
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
   * Find the best matching StellarLibrarySpectrum for given spectral classes. Uses physics-based
   * scoring from Stephens match_sed.py reference implementation.
   */
  private def findBestStellarMatch(
    luminosityClasses:  List[String],
    temperatureClasses: List[String]
  ): Option[StellarLibrarySpectrum] =

    // Handle edge cases upfront - match Python behavior: return None if no classes
    if temperatureClasses.isEmpty && luminosityClasses.isEmpty then None
    else
      // Calculate physical parameters for target star
      StellarPhysics.calculateParameters(luminosityClasses, temperatureClasses) match {
        case Some(targetParams) =>
          // Score all library SEDs based on physical parameters
          val scored = StellarLibrarySpectrum.values.toList.flatMap { spectrum =>
            StellarLibraryParameters.getParameters(spectrum).map { sedParams =>
              // Calculate differences - convert quantities to raw values for arithmetic
              val dtValue   = sedParams.tEff.value - targetParams.tEff.value
              val dg        = sedParams.logG - targetParams.logG
              val dtMax     = TemperatureToleranceFraction * targetParams.tEff.value
              val dgMax     = GravityToleranceDex

              // Calculate score: sqrt((ΔT/ΔT_max)² + (Δlog_g/Δlog_g_max)²)
              val score = math.sqrt((dtValue / dtMax) * (dtValue / dtMax) + (dg / dgMax) * (dg / dgMax))

              (spectrum, score, math.abs(dtValue), dtMax, math.abs(dg), dgMax)
            }
          }

          // Return best match (lowest score) if BOTH differences are within tolerance
          // This matches Python logic: abs(dt) < dt_max AND abs(dg) < dg_max
          scored
            .sortBy(_._2)
            .headOption
            .filter { case (_, _, absDt, dtMax, absDg, dgMax) =>
              absDt < dtMax && absDg < dgMax
            }
            .map(_._1)

        case None =>
          // Cannot calculate parameters - match Python behavior: return None
          None
      }

  /**
   * Match galaxy morphological type to appropriate GalaxySpectrum.
   */
  private def matchGalaxySED(morphType: String): EitherNec[CatalogProblem, UnnormalizedSED] = {
    // Try elliptical pattern: E optionally followed by digit/colon/plus
    if (morphType.matches(EllipticalPattern)) {
      Right(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    }
    // Try S0 pattern (lenticular, classified as elliptical)
    else if (morphType.matches(S0Pattern)) {
      Right(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
    }
    // Try spiral pattern: S optionally followed by a/b/c (upper/lower)
    else if (morphType.matches(SpiralPattern)) {
      Right(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
    }
    // Try numerical Hubble stage classification
    else {
      morphType.toDoubleOption
        .flatMap { hubbleStage =>
          if (hubbleStage <= EllipticalHubbleStageThreshold) Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
          else if (hubbleStage < SpiralHubbleStageThreshold) Some(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
          else None
        }
        .toRight(NonEmptyChain.one(InvalidMorphologicalType(morphType)))
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
