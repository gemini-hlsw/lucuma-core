// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.option.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import lucuma.core.syntax.string.*

/**
 * Stellar physics calculations for matching spectral types to library SEDs. Based on the Python
 * reference implementation (match_sed.py by Andrew Stephens).
 *
 * References:
 *   - Temperature: Malkov et al, 2020, RAA, 20, 139
 *   - Gravity: Straizys & Kuriliene, 1981, Ap&SS, 80, 353S
 */
class StellarPhysics(gravityTableConfig: GravityTableConfig):

  import StellarPhysics.*

  private val gravityTable = gravityTableConfig.rows

  private[votable] def calculateGravity(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[Double] =
    (luminosity, temperature) match
      case (Nil, _) | (_, Nil) | (Nil, Nil)   =>
        none
      case ((h :: _), t) if h.startsWith("D") =>
        WhiteDwarfLogG.some
      case _                                  =>
        val gravities = for {
          lc  <- luminosity
          tc  <- temperature
          scc <- spectralClassCode(tc)
          if scc < BrownDwarfThreshold
        } yield
          val normalizedLC = lc match
            case "I"                                     => "Iab"
            case "VI"                                    => "sd"
            case s if Supergiants.contains(s)            => s
            case s if s.endsWith("a") || s.endsWith("b") => s.dropRight(1)
            case s                                       => s

          interpolateGravity(scc, normalizedLC)

        gravities match
          case Nil => none
          case _   =>
            BigDecimal(gravities.sum / gravities.length)
              .setScale(3, BigDecimal.RoundingMode.HALF_UP)
              .toDouble
              .some

  private def interpolateGravity(scc: Double, lumClass: String): Double =
    val entriesWithLC = gravityTable.filter(_._2.contains(lumClass))
    val tableToUse    =
      if entriesWithLC.nonEmpty then entriesWithLC
      else gravityTable.filter(_._2.contains("V"))

    if tableToUse.isEmpty then DefaultLogG
    else
      tableToUse.partition(_._1 <= scc) match
        case (lows, highs) if lows.nonEmpty && highs.nonEmpty =>
          val (sccLow, mapLow)   = lows.last
          val (sccHigh, mapHigh) = highs.head
          interpolate(scc, sccLow, mapLow(lumClass), sccHigh, mapHigh(lumClass))
        case (lows, _) if lows.nonEmpty                       =>
          lows.last._2(lumClass)
        case (_, highs) if highs.nonEmpty                     =>
          highs.head._2(lumClass)
        case _                                                =>
          DefaultLogG

  private def interpolate(
    scc:      Double,
    sccLow:   Double,
    logGLow:  Double,
    sccHigh:  Double,
    logGHigh: Double
  ): Double =
    if sccLow == sccHigh then logGLow
    else
      val fraction = (scc - sccLow) / (sccHigh - sccLow)
      logGLow + fraction * (logGHigh - logGLow)

  private[votable] def calculateParameters(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[StellarParameters] =
    for {
      temp <- calculateTemperature(luminosity, temperature)
      logG <- calculateGravity(luminosity, temperature)
    } yield StellarParameters(temp.withUnit[Kelvin], logG)

object StellarPhysics:

  private val BrownDwarfThreshold = 70.0
  private val WhiteDwarfLogG      = 8.0
  private val Supergiants         = Set("Ia", "Ib", "Iab")
  private val DefaultLogG         = 4.0
  // Default subclass when spectral type lacks a number (e.g., "G" -> "G5")
  private val DefaultSubclass     = 5.0

  private val SpectralClassCodes =
    Map(
      'O' -> 0.0,
      'B' -> 10.0,
      'A' -> 20.0,
      'F' -> 30.0,
      'G' -> 40.0,
      'K' -> 50.0,
      'M' -> 60.0,
      'L' -> 70.0,
      'T' -> 80.0,
      'Y' -> 90.0
    )

  case class StellarParameters(temp: Quantity[Int, Kelvin], logG: Double)

  private[votable] def spectralClassCode(spectralClass: String): Option[Double] =
    spectralClass.headOption.flatMap: letter =>
      SpectralClassCodes
        .get(letter)
        .map: baseCode =>
          val subclass = SpectralTypeParsers.tempSubclass.parse(spectralClass.tail) match
            case Right((_, value)) => value.parseDoubleOption.getOrElse(DefaultSubclass)
            case Left(_)           => DefaultSubclass

          // +/- modifiers shift by quarter subclass
          val modifier =
            if spectralClass.lastOption.contains('+') then 0.25
            else if spectralClass.lastOption.contains('-') then -0.25
            else 0.0

          baseCode + subclass + modifier

  private[votable] def calculateTemperature(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[Int] =
    (luminosity, temperature) match
      case (Nil, Nil) | (_, Nil) | (Nil, _)   => none
      case ((h :: _), t) if h.startsWith("D") =>
        // White dwarf temperature: T_eff = 50400K / n for type DAn
        t.headOption.flatMap: tc =>
          tc.parseDoubleOption.map(num => (50400.0 / num).round.toInt)
      case _                                  =>
        val temps = temperature
          .flatMap: tc =>
            spectralClassCode(tc).flatMap:
              case scc if scc >= BrownDwarfThreshold => None
              case scc                               =>
                // Polynomial fit from Malkov et al, 2020, RAA, 20, 139
                val logTEff =
                  5.07073 - 7.57056e-2 * scc + 1.47089e-3 * scc * scc - 1.03905e-5 * scc * scc * scc
                math.pow(10, logTEff).some
        // Average temperatures when multiple classes given
        temps match
          case Nil => none
          case t   => (t.sum / t.length).round.toInt.some
