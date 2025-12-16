// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.syntax.option.*
import lucuma.catalog.votable.CatalogProblem.*
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.string.*

private enum ObjectCategory:
  case Star, Galaxy, Quasar, HIIRegion, PlanetaryNebula

private case class ScoredMatch(
  spectrum: StellarLibrarySpectrum,
  score:    Double,
  absDt:    Double,
  dtMax:    Double,
  absDg:    Double,
  dgMax:    Double
):
  def isWithinTolerance: Boolean = absDt < dtMax && absDg < dgMax

object SEDMatcher:
  // Stellar matching tolerances (physics-based scoring)
  private val TemperatureToleranceFraction: Double = 0.1 // 10% of target temperature
  private val GravityToleranceDex: Double          = 0.5 // 0.5 dex in log(g)

  // Galaxy Hubble stage classification boundaries
  private val EllipticalHubbleStageThreshold: Double = -0.5 // E0-S0
  private val SpiralHubbleStageThreshold: Double     = 9.0  // Sa-Sm

  // Galaxy morphological type patterns (as Regex for pattern matching)
  private val EllipticalPattern = """E[0-9:+]?.*""".r
  private val S0Pattern         = """S0.*""".r
  private val SpiralPattern     = """S[abcABC_:]?.*""".r

  /**
   * Attempt to Infer an appropriate UnnormalizedSED from Simbad object classification.
   *
   * Based on match_sed python package by Andy Stephens
   *
   * Uses physics-based matching for stars and morphological patterns for galaxies. Quasars, HII
   * regions, and planetary nebulae are assigned fixed spectra.
   *
   * For stars, matching is performed by comparing target stellar parameters (calculated from
   * spectral type) against all library SEDs. The match must satisfy both temperature (±10%) and
   * gravity (±0.5 dex) tolerances.
   *
   * @param otype
   *   Simbad object type code (e.g., "*", "G", "QSO"). See [[StarTypes]], [[GalaxyTypes]],
   *   [[QuasarTypes]], [[HIITypes]], [[PNTypes]].
   * @param spectralType
   *   Optional spectral classification string (e.g., "G2V", "K3III", "DA3"). Required for stellar
   *   objects to perform physics-based matching.
   * @param morphType
   *   Optional morphological type for galaxies (e.g., "Sa", "E3", or Hubble stage like "2.0").
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
          .toRight(NonEmptyChain.one(InvalidSpectralType(spectralType.orEmpty)))
          .flatMap(matchStellarSED)
      case Some(ObjectCategory.Galaxy)          =>
        morphType
          .toRight(NonEmptyChain.one(InvalidMorphologicalType(morphType.orEmpty)))
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
   * match simbad_otype mappings from as in match_sed.py
   *
   * https://simbad.u-strasbg.fr/Pages/guide/otypes.htx Simbad Object Types
   */
  private def parseObjectType(otype: String): Option[ObjectCategory] =
    if (StarTypes.contains(otype)) ObjectCategory.Star.some
    else if (GalaxyTypes.contains(otype)) ObjectCategory.Galaxy.some
    else if (QuasarTypes.contains(otype)) ObjectCategory.Quasar.some
    else if (HIITypes.contains(otype)) ObjectCategory.HIIRegion.some
    else if (PNTypes.contains(otype)) ObjectCategory.PlanetaryNebula.some
    else none

  /**
   * Match a stellar spectral type to StellarLibrarySpectrum.
   */
  private def matchStellarSED(spectralType: String): EitherNec[CatalogProblem, UnnormalizedSED] =
    parseSpectralType(spectralType) match {
      case Some((luminosity, temperature)) =>
        findBestStellarMatch(luminosity, temperature)
          .map(UnnormalizedSED.StellarLibrary(_))
          .toRight(NonEmptyChain.one(UnmatchedSpectralType(spectralType)))
      case _                               =>
        Left(NonEmptyChain.one(InvalidSpectralType(spectralType)))
    }

  /**
   * Parse Simbad spectral type string into luminosity and temperature classes. Uses parser
   * combinators for robust parsing.
   */
  private def parseSpectralType(spectralType: String): Option[(List[String], List[String])] =
    // Clean up some chars used by simbad
    // https://simbad.cds.unistra.fr/guide/chD.htx
    val cleaned = spectralType.replaceAll("[():]", "")
    SpectralTypeParsers.spectralType.parse(cleaned).toOption.map(_._2)

  /**
   * Find the best matching StellarLibrarySpectrum for given spectral classes. Uses physics-based
   * scoring
   */
  private def findBestStellarMatch(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[StellarLibrarySpectrum] =

    // Handle edge cases upfront - match Python behavior: return None if no classes
    if luminosity.isEmpty && temperature.isEmpty then none
    else
      StellarPhysics
        .calculateParameters(luminosity, temperature)
        .flatMap: params =>
          StellarLibrarySpectrum.values.toList
            .flatMap(scoreSpectrum(params))
            .sortBy(_.score)
            .headOption
            .filter(_.isWithinTolerance)
            .map(_.spectrum)

  private def scoreSpectrum(targetParams: StellarPhysics.StellarParameters)(
    spectrum: StellarLibrarySpectrum
  ): Option[ScoredMatch] =
    StellarLibraryParameters.getParameters(spectrum).map { sedParams =>
      // Calculate differences
      val dtValue = sedParams.tEff.value - targetParams.tEff.value
      val dg      = sedParams.logG - targetParams.logG
      val dtMax   = TemperatureToleranceFraction * targetParams.tEff.value
      val dgMax   = GravityToleranceDex

      // Calculate score: sqrt((ΔT/ΔT_max)² + (Δlog_g/Δlog_g_max)²)
      val score = math.sqrt((dtValue / dtMax) * (dtValue / dtMax) + (dg / dgMax) * (dg / dgMax))

      ScoredMatch(spectrum, score, math.abs(dtValue), dtMax, math.abs(dg), dgMax)
    }

  /**
   * Match galaxy morphological type to appropriate GalaxySpectrum.
   */
  private def matchGalaxySED(morphType: String): EitherNec[CatalogProblem, UnnormalizedSED] =
    morphType match
      case EllipticalPattern() => Right(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case S0Pattern()         => Right(UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical))
      case SpiralPattern()     => Right(UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral))
      case hubble              =>
        hubble.parseDoubleOption
          .flatMap:
            case stage if stage <= EllipticalHubbleStageThreshold =>
              UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical).some
            case stage if stage <= SpiralHubbleStageThreshold     =>
              UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral).some
            case _                                                =>
              none
          .toRight(NonEmptyChain.one(InvalidMorphologicalType(morphType)))

// TODO Consider making these external
val GalaxyTypes = Set("G",
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

val QuasarTypes =
  Set("AGN", "AG?", "SyG", "Sy1", "Sy2", "QSO", "Q?", "Bla", "Bz?", "BLL", "BL?", "LIN")

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
