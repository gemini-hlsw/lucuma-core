// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.data.EitherNec
import cats.data.NonEmptyChain
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.catalog.votable.CatalogProblem
import lucuma.catalog.votable.CatalogProblem.*
import lucuma.core.enums.*
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.string.*
import lucuma.core.util.Enumerated

/**
 * SED matching based on match_sed Python package by Andy Stephens.
 *
 * Uses physics-based matching for stars and morphological patterns for galaxies. Quasars, HII
 * regions, and planetary nebulae use fixed SED templates.
 */
class SEDMatcher(
  stellarLibrary: StellarLibraryParameters,
  physics:        StellarPhysics
):

  import SEDMatcher.*

  /**
   * Attempt to get an appropriate UnnormalizedSED from Simbad object classification.
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

  private def matchStellarSED(spectralType: String): EitherNec[CatalogProblem, UnnormalizedSED] =
    parseSpectralType(spectralType) match {
      case Some((luminosity, temperature)) =>
        matchingSpectrum(luminosity, temperature)
          .map(UnnormalizedSED.StellarLibrary(_))
          .toRight(NonEmptyChain.one(UnmatchedSpectralType(spectralType)))
      case _                               =>
        Left(NonEmptyChain.one(InvalidSpectralType(spectralType)))
    }

  // Finds the best matching spectrum
  private def matchingSpectrum(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[StellarLibrarySpectrum] =

    (luminosity, temperature) match
      case (Nil, Nil) | (_, Nil) | (Nil, _) => none
      case (l, t)                           =>
        physics
          .calculateParameters(l, t)
          .flatMap: params =>
            val targetHasGiantClass = l.exists(GiantClasses.contains)
            val targetHasDwarfClass = l.exists(c => c === "V" || c === "IV")
            val needsStrictGravity  = targetHasGiantClass && !targetHasDwarfClass

            val compatibleMatches = stellarLibrary.preferredSpectraOrdered
              .filter(s => luminosityCompatible(l, libraryLuminosity(s)))
              .flatMap(scoreSpectrum(params))
              .sortBy(m => (m.score, stellarLibrary.fileOrderIndex(m.spectrum)))

            val bestMatch = compatibleMatches.headOption.filter(_.isWithinTolerance)

            bestMatch
              .flatMap: m =>
                val libLum       = libraryLuminosity(m.spectrum)
                val isVMatch     = libLum.contains("V") && !libLum.exists(GiantClasses.contains)
                val rejectVMatch =
                  needsStrictGravity && isVMatch && m.absDg >= CrossLuminosityGravityTolerance

                Option.unless(rejectVMatch)(m.spectrum)
              .orElse:
                val targetCat = categorizeLuminosity(l)

                Option
                  .when(targetCat === LuminosityCategory.Subdwarf):
                    stellarLibrary.preferredSpectraOrdered
                      .filter(s =>
                        categorizeLuminosity(libraryLuminosity(s)) === LuminosityCategory.Normal
                      )
                      .flatMap(scoreSpectrum(params))
                      .sortBy(m => (m.score, stellarLibrary.fileOrderIndex(m.spectrum)))
                      .headOption
                      .filter(_.isWithinTolerance)
                      .map(_.spectrum)
                  .flatten

  private def scoreSpectrum(targetParams: StellarPhysics.StellarParameters)(
    spectrum: StellarLibrarySpectrum
  ): Option[ScoredMatch] =
    Option
      .unless(targetParams.temp.value <= 0):
        stellarLibrary.params
          .get(spectrum)
          .map: sedParams =>
            val dtValue = sedParams.temp.value - targetParams.temp.value
            val dg      = sedParams.logG - targetParams.logG
            val dtMax   = TemperatureToleranceFraction * targetParams.temp.value
            val dgMax   = GravityToleranceDex

            val score =
              math.sqrt((dtValue / dtMax) * (dtValue / dtMax) + (dg / dgMax) * (dg / dgMax))

            ScoredMatch(spectrum, score, math.abs(dtValue), dtMax, math.abs(dg), dgMax)
      .flatten

  private inline def libraryLuminosity(spectrum: StellarLibrarySpectrum): List[String] =
    stellarLibrary.luminosityClasses(spectrum)

object SEDMatcher:
  def fromConfig(config: SEDDataConfig): SEDMatcher =
    val physics = new StellarPhysics(config.gravityTable)
    val library = new StellarLibraryParameters(config.stellarLibrary, physics)
    new SEDMatcher(library, physics)

  private enum ObjectCategory(val tag: String) derives Enumerated:
    case Star            extends ObjectCategory("star")
    case Galaxy          extends ObjectCategory("galaxy")
    case Quasar          extends ObjectCategory("quasar")
    case HIIRegion       extends ObjectCategory("hii_region")
    case PlanetaryNebula extends ObjectCategory("plantery_nebula")

  private enum LuminosityCategory(val tag: String) derives Enumerated:
    case Subdwarf   extends LuminosityCategory("subdwarf")
    case WhiteDwarf extends LuminosityCategory("whitedwarf")
    case Normal     extends LuminosityCategory("normal")

  private case class ScoredMatch(
    spectrum: StellarLibrarySpectrum,
    score:    Double,
    absDt:    Double,
    dtMax:    Double,
    absDg:    Double,
    dgMax:    Double
  ):
    def isWithinTolerance: Boolean = absDt < dtMax && absDg < dgMax

  // Tolerance factors are unitless
  private val TemperatureToleranceFraction: Double    = 0.1
  private val GravityToleranceDex: Double             = 0.5
  private val CrossLuminosityGravityTolerance: Double = 0.4
  private val EllipticalHubbleStageThreshold: Double  = -0.5

  private val GiantClasses: Set[String] = Set("I", "Ia", "Iab", "Ib", "II", "III")

  // Some regexes to match galaxy SEDs
  private val EllipticalPattern = """E[0-9:+]?.*""".r
  private val S0Pattern         = """S[0O].*""".r
  private val SpiralPattern     = """S[abcABC_:]?.*""".r

  private val NebularNonVPattern = """.*I{1,3}n.*""".r

  private def parseObjectType(otype: String): Option[ObjectCategory] =
    if (StarTypes.contains(otype)) ObjectCategory.Star.some
    else if (GalaxyTypes.contains(otype)) ObjectCategory.Galaxy.some
    else if (QuasarTypes.contains(otype)) ObjectCategory.Quasar.some
    else if (HIITypes.contains(otype)) ObjectCategory.HIIRegion.some
    else if (PNTypes.contains(otype)) ObjectCategory.PlanetaryNebula.some
    else none

  private def parseSpectralType(spectralType: String): Option[(List[String], List[String])] =
    val clean = spectralType.replaceAll("[():]", "")

    SpectralTypeParsers.spectralType
      .parse(clean)
      .toOption
      .map:
        case (_, (lum, temp)) =>
          val adjustedTemp =
            if spectralType.matches(NebularNonVPattern.regex) then
              temp.map(tc => tc.replaceAll("""\.\d+""", ""))
            else temp
          (lum, adjustedTemp)

  private def categorizeLuminosity(lumClasses: List[String]): LuminosityCategory =
    if lumClasses.exists(l => l === "sd" || l === "VI") then LuminosityCategory.Subdwarf
    else if lumClasses.exists(_.startsWith("D")) then LuminosityCategory.WhiteDwarf
    else LuminosityCategory.Normal

  private def luminosityCompatible(
    targetLum:  List[String],
    libraryLum: List[String]
  ): Boolean =
    val targetCat  = categorizeLuminosity(targetLum)
    val libraryCat = categorizeLuminosity(libraryLum)

    targetCat =!= LuminosityCategory.WhiteDwarf && libraryCat =!= LuminosityCategory.WhiteDwarf ||
    targetCat === libraryCat

  private def matchGalaxySED(morphType: String): EitherNec[CatalogProblem, UnnormalizedSED] =
    morphType match
      case EllipticalPattern() => UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical).asRight
      case S0Pattern()         => UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical).asRight
      case SpiralPattern()     => UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral).asRight
      case hubble              =>
        hubble.parseDoubleOption
          .flatMap:
            case stage if stage <= EllipticalHubbleStageThreshold =>
              UnnormalizedSED.Galaxy(GalaxySpectrum.Elliptical).some
            case _                                                =>
              UnnormalizedSED.Galaxy(GalaxySpectrum.Spiral).some
          .toRight(NonEmptyChain.one(InvalidMorphologicalType(morphType)))

  // Condensed codes and their OTYPE_S labels from the Simbad taxonomy.
  // Labels sourced from https://simbad.cds.unistra.fr/guide/otypes.labels.txt
  // format: off
  private val GalaxyTypes = Set(
    "G", "Galaxy", "GGG", "LSB", "LowSurfBrghtG", "bCG", "BlueCompactG", "SBG", "StarburstG",
    "H2G", "HIIG", "EmG", "EmissionG", "rG", "RadioG", "GiP", "GinPair", "GiG", "GtowardsGroup",
    "GiC", "GtowardsCl", "BiC", "BrightestCG", "IG", "InteractingG", "PaG", "PairG",
    "GrG", "GroupG", "CGG", "Compact_Gr_G", "CIG", "PCG", "SCG", "SuperClG")

  private val QuasarTypes = Set(
    "AGN", "AG?", "AGN_Candidate", "SyG", "Seyfert", "Sy1", "Seyfert1", "Sy2", "Seyfert2",
    "QSO", "Q?", "QSO_Candidate", "Bla", "Blazar", "Bz?", "Blazar_Candidate",
    "BLL", "BLLac", "BL?", "BLLac_Candidate", "LIN", "LINER")

  private val HIITypes = Set("HII", "HIIReg")
  private val PNTypes  = Set("PN", "PlanetaryNeb")

  private val StarTypes = Set(
    "*", "Star", "Ma*", "bC*", "bCepV*", "sg*", "Supergiant", "s*r", "RedSG", "s*y", "YellowSG",
    "s*b", "BlueSG", "WR*", "WolfRayet*", "N*", "Neutron*", "Psr", "Pulsar", "Y*O", "YSO",
    "Or*", "OrionV*", "TT*", "TTauri*", "Ae*", "HH", "HerbigHaroObj", "MS*", "MainSequence*", "Be*", "BS*",
    "BlueStraggler", "SX*", "SXPheV*", "gD*", "gammaDorV*", "dS*", "delSctV*", "PulsV*delSct",
    "Ev*", "RG*", "RGB*", "HS*", "HotSubdwarf", "HB*", "HorBranch*", "RR*", "RRLyrae",
    "WV*", "Type2Cep", "Ce*", "Cepheid", "cC*", "ClassicalCep", "C*", "S*", "LP*", "LongPeriodV*",
    "AB*", "AGB*", "Mi*", "Mira", "OH*", "OH/IR*", "pA*", "post-AGB*", "RV*", "RVTauV*",
    "WD*", "WhiteDwarf", "Pe*", "ChemPec*", "a2*", "alf2CVnV*", "RC*", "RCrBV*", "LM*", "Low-Mass*",
    "BD*", "BrownD*", "Ir*", "IrregularV*", "Er*", "Eruptive*", "Ro*", "RotV*", "Pu*", "PulsV*",
    "Em*", "EmLine*", "PM*", "HighPM*", "HV*", "HighVel*")
  // format: on
