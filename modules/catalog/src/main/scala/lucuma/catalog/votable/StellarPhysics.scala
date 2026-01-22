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
object StellarPhysics:

  // Brown dwarfs (L, T, Y) have spectral class codes >= 70
  private val BrownDwarfThreshold: Double = 70.0

  // White dwarfs have extremely high surface gravity (solar mass in Earth-sized volume)
  private val WhiteDwarfLogG: Double = 8.0

  private val Supergiants: Set[String] = Set("Ia", "Ib", "Iab")

  private val DefaultLogG: Double = 4.0

  private val SpectralClassCodes = Map(
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

  case class StellarParameters(
    temp: Quantity[Int, Kelvin], // Effective temperature
    logG: Double                 // Log of surface gravity
  )

  /**
   * Convert a spectral class string to a numerical code.
   *
   * Maps spectral type letters to base values (O=0, B=10, A=20, ..., Y=90) and adds subclass digits
   * (0-9.5).
   */
  def spectralClassCode(spectralClass: String): Option[Double] =
    Option
      .when(spectralClass.nonEmpty):
        val letter = spectralClass.head
        SpectralClassCodes
          .get(letter)
          .flatMap: baseCode =>
            val subclass = SpectralTypeParsers.tempSubclass.parse(spectralClass.tail) match
              case Right((_, value)) =>
                value.parseDoubleOption.getOrElse(5.0)
              case Left(_)           =>
                5.0 // Default to 5 if no subclass specified

            // Handle modifiers on the spectral class
            // '+' and '-' count as a quarter of a subclass (Keenan & McNeil)
            val modifier =
              if spectralClass.contains('+') then 0.25
              else if spectralClass.contains('-') then -0.25
              else 0.0

            Some(baseCode + subclass + modifier)
      .flatten

  /**
   * Calculate effective temperature from stellar spectral classification.
   *
   * Uses polynomial relation from Malkov et al, 2020, RAA, 20, 139 for main sequence stars. White
   * dwarfs use T_eff = 50400 / number formula. Returns average if multiple temperature classes
   * provided.
   */
  def calculateTemperature(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[Int] =
    (luminosity, temperature) match
      case (Nil, Nil) | (_, Nil) | (Nil, _)   => none
      // White dwarfs luminosity start with D
      case ((h :: _), t) if h.startsWith("D") =>
        // white dwarfs: T_eff = 50400 / number (Sion et al.)
        t.headOption.flatMap: tc =>
          tc.parseDoubleOption.map(num => (50400.0 / num).round.toInt)
      case _                                  =>
        // Calculate temperature for each temperature class (no rounding until the end)
        val temps = temperature
          .flatMap: tc =>
            spectralClassCode(tc).flatMap:
              case scc if scc >= BrownDwarfThreshold => None
              case scc                               =>
                // Polynomial from Malkov et al.
                val logTEff =
                  5.07073 - 7.57056e-2 * scc + 1.47089e-3 * scc * scc - 1.03905e-5 * scc * scc * scc
                math.pow(10, logTEff).some
        temps match
          case Nil => none
          // average across temperatures, round only the final result
          case t   => (t.sum / t.length).round.toInt.some

  /**
   * Gravity interpolation table from Straizys & Kuriliene, 1981, Ap&SS, 80, 353S. Extended with
   * data from Malkov et al, 2020, RAA, 20, 139. TODO Externalize this to a file
   */
  private val GravityTable: List[(Double, Map[String, Double])] = List(
    (1.0,
     Map("V"   -> 3.90,
         "IV"  -> 3.85,
         "III" -> 3.80,
         "II"  -> 4.05,
         "Ib"  -> 4.00,
         "Iab" -> 3.90,
         "Ia"  -> 3.80,
         "sd"  -> 4.5
     )
    ),
    (5.0, // O5
     Map("V"   -> 3.90,
         "IV"  -> 3.86,
         "III" -> 3.82,
         "II"  -> 3.76,
         "Ib"  -> 3.74,
         "Iab" -> 3.69,
         "Ia"  -> 3.57
     )
    ),
    (6.0, // O6
     Map("V"   -> 3.86,
         "IV"  -> 3.80,
         "III" -> 3.76,
         "II"  -> 3.69,
         "Ib"  -> 3.64,
         "Iab" -> 3.60,
         "Ia"  -> 3.53
     )
    ),
    (7.0, // O7
     Map("V"   -> 3.85,
         "IV"  -> 3.80,
         "III" -> 3.74,
         "II"  -> 3.64,
         "Ib"  -> 3.57,
         "Iab" -> 3.52,
         "Ia"  -> 3.45
     )
    ),
    (8.0, // O8
     Map("V"   -> 3.87,
         "IV"  -> 3.81,
         "III" -> 3.75,
         "II"  -> 3.62,
         "Ib"  -> 3.53,
         "Iab" -> 3.49,
         "Ia"  -> 3.39
     )
    ),
    (9.0, // O9
     Map("V"   -> 3.95,
         "IV"  -> 3.82,
         "III" -> 3.74,
         "II"  -> 3.58,
         "Ib"  -> 3.50,
         "Iab" -> 3.44,
         "Ia"  -> 3.31
     )
    ),
    (10.0, // B0
     Map("V"   -> 4.00,
         "IV"  -> 3.88,
         "III" -> 3.74,
         "II"  -> 3.39,
         "Ib"  -> 3.27,
         "Iab" -> 3.19,
         "Ia"  -> 3.05,
         "sd"  -> 4.5
     )
    ),
    (11.0, // B1
     Map("V"   -> 4.00,
         "IV"  -> 3.86,
         "III" -> 3.71,
         "II"  -> 3.31,
         "Ib"  -> 3.17,
         "Iab" -> 3.01,
         "Ia"  -> 2.87
     )
    ),
    (12.0, // B2
     Map("V"   -> 4.06,
         "IV"  -> 3.88,
         "III" -> 3.68,
         "II"  -> 3.19,
         "Ib"  -> 3.00,
         "Iab" -> 2.84,
         "Ia"  -> 2.68
     )
    ),
    (13.0, // B3
     Map("V"   -> 4.06,
         "IV"  -> 3.89,
         "III" -> 3.71,
         "II"  -> 3.12,
         "Ib"  -> 2.79,
         "Iab" -> 2.68,
         "Ia"  -> 2.49
     )
    ),
    (15.0, // B5
     Map("V"   -> 4.10,
         "IV"  -> 3.98,
         "III" -> 3.81,
         "II"  -> 2.90,
         "Ib"  -> 2.52,
         "Iab" -> 2.40,
         "Ia"  -> 2.22
     )
    ),
    (16.0, // B6
     Map("V"   -> 4.09,
         "IV"  -> 3.96,
         "III" -> 3.84,
         "II"  -> 2.77,
         "Ib"  -> 2.42,
         "Iab" -> 2.29,
         "Ia"  -> 2.13
     )
    ),
    (17.0, // B7
     Map("V"   -> 4.07,
         "IV"  -> 3.95,
         "III" -> 3.82,
         "II"  -> 2.77,
         "Ib"  -> 2.33,
         "Iab" -> 2.21,
         "Ia"  -> 2.02
     )
    ),
    (18.0, // B8
     Map("V"   -> 4.07,
         "IV"  -> 3.92,
         "III" -> 3.79,
         "II"  -> 2.79,
         "Ib"  -> 2.27,
         "Iab" -> 2.11,
         "Ia"  -> 1.97
     )
    ),
    (19.0, // B9
     Map("V"   -> 4.03,
         "IV"  -> 3.94,
         "III" -> 3.75,
         "II"  -> 2.81,
         "Ib"  -> 2.20,
         "Iab" -> 2.04,
         "Ia"  -> 1.88
     )
    ),
    (20.0, // A0
     Map("V"   -> 4.07,
         "IV"  -> 3.91,
         "III" -> 3.75,
         "II"  -> 2.85,
         "Ib"  -> 2.23,
         "Iab" -> 2.01,
         "Ia"  -> 1.81,
         "sd"  -> 4.5
     )
    ),
    (21.0, // A1
     Map("V"   -> 4.10,
         "IV"  -> 3.96,
         "III" -> 3.78,
         "II"  -> 2.88,
         "Ib"  -> 2.22,
         "Iab" -> 1.96,
         "Ia"  -> 1.76
     )
    ),
    (22.0, // A2
     Map("V"   -> 4.16,
         "IV"  -> 3.98,
         "III" -> 3.78,
         "II"  -> 2.87,
         "Ib"  -> 2.23,
         "Iab" -> 1.92,
         "Ia"  -> 1.73
     )
    ),
    (23.0, // A3
     Map("V"   -> 4.20,
         "IV"  -> 4.03,
         "III" -> 3.83,
         "II"  -> 2.85,
         "Ib"  -> 2.20,
         "Iab" -> 1.86,
         "Ia"  -> 1.65
     )
    ),
    (25.0, // A5
     Map("V"   -> 4.22,
         "IV"  -> 4.06,
         "III" -> 3.86,
         "II"  -> 2.81,
         "Ib"  -> 2.14,
         "Iab" -> 1.74,
         "Ia"  -> 1.53
     )
    ),
    (27.0, // A7
     Map("V"   -> 4.26,
         "IV"  -> 4.10,
         "III" -> 3.86,
         "II"  -> 2.75,
         "Ib"  -> 2.08,
         "Iab" -> 1.65,
         "Ia"  -> 1.38
     )
    ),
    (30.0, // F0
     Map("V"   -> 4.28,
         "IV"  -> 4.05,
         "III" -> 3.83,
         "II"  -> 2.67,
         "Ib"  -> 2.00,
         "Iab" -> 1.51,
         "Ia"  -> 1.25,
         "sd"  -> 4.0
     )
    ),
    (32.0, // F2
     Map("V"   -> 4.26,
         "IV"  -> 4.01,
         "III" -> 3.81,
         "II"  -> 2.63,
         "Ib"  -> 1.92,
         "Iab" -> 1.39,
         "Ia"  -> 1.15
     )
    ),
    (35.0, // F5
     Map("V"   -> 4.28,
         "IV"  -> 3.93,
         "III" -> 3.74,
         "II"  -> 2.48,
         "Ib"  -> 1.81,
         "Iab" -> 1.22,
         "Ia"  -> 1.00
     )
    ),
    (38.0, // F8
     Map("V"   -> 4.35,
         "IV"  -> 3.89,
         "III" -> 3.53,
         "II"  -> 2.38,
         "Ib"  -> 1.71,
         "Iab" -> 1.06,
         "Ia"  -> 0.83
     )
    ),
    (40.0, // G0
     Map("V"   -> 4.39,
         "IV"  -> 3.84,
         "III" -> 3.35,
         "II"  -> 2.29,
         "Ib"  -> 1.62,
         "Iab" -> 0.95,
         "Ia"  -> 0.72,
         "sd"  -> 3.5
     )
    ),
    (42.0, // G2
     Map("V"   -> 4.40,
         "IV"  -> 3.77,
         "III" -> 3.20,
         "II"  -> 2.20,
         "Ib"  -> 1.53,
         "Iab" -> 0.86,
         "Ia"  -> 0.61
     )
    ),
    (45.0, // G5
     Map("V"   -> 4.49,
         "IV"  -> 3.71,
         "III" -> 3.07,
         "II"  -> 2.04,
         "Ib"  -> 1.45,
         "Iab" -> 0.71,
         "Ia"  -> 0.45
     )
    ),
    (48.0, // G8
     Map("V"   -> 4.55,
         "IV"  -> 3.64,
         "III" -> 2.95,
         "II"  -> 1.84,
         "Ib"  -> 1.30,
         "Iab" -> 0.60,
         "Ia"  -> 0.30
     )
    ),
    (50.0, // K0
     Map("V"   -> 4.57,
         "IV"  -> 3.57,
         "III" -> 2.89,
         "II"  -> 1.74,
         "Ib"  -> 1.20,
         "Iab" -> 0.54,
         "Ia"  -> 0.25,
         "sd"  -> 3.0
     )
    ),
    (51.0, // K1
     Map("V"   -> 4.55,
         "IV"  -> 3.55,
         "III" -> 2.78,
         "II"  -> 1.66,
         "Ib"  -> 1.16,
         "Iab" -> 0.54,
         "Ia"  -> 0.25
     )
    ),
    (52.0, // K2
     Map("V"   -> 4.55,
         "IV"  -> 3.54,
         "III" -> 2.63,
         "II"  -> 1.59,
         "Ib"  -> 1.10,
         "Iab" -> 0.48,
         "Ia"  -> 0.23
     )
    ),
    (53.0, // K3
     Map("V"   -> 4.56,
         "IV"  -> 3.52,
         "III" -> 2.36,
         "II"  -> 1.52,
         "Ib"  -> 1.00,
         "Iab" -> 0.46,
         "Ia"  -> 0.19
     )
    ),
    (54.0, // K4
     Map("V"   -> 4.57,
         "IV"  -> 3.49,
         "III" -> 2.16,
         "II"  -> 1.35,
         "Ib"  -> 0.90,
         "Iab" -> 0.41,
         "Ia"  -> 0.15
     )
    ),
    (55.0, // K5
     Map("V"   -> 4.57,
         "IV"  -> 3.47,
         "III" -> 1.93,
         "II"  -> 1.20,
         "Ib"  -> 0.77,
         "Iab" -> 0.35,
         "Ia"  -> 0.10
     )
    ),
    (57.0, // K7
     Map("V"   -> 4.62,
         "IV"  -> 3.42,
         "III" -> 1.80,
         "II"  -> 1.12,
         "Ib"  -> 0.69,
         "Iab" -> 0.33,
         "Ia"  -> 0.04
     )
    ),
    (60.0, // M0
     Map("V"   -> 4.61,
         "IV"  -> 3.35,
         "III" -> 1.63,
         "II"  -> 1.01,
         "Ib"  -> 0.61,
         "Iab" -> 0.30,
         "Ia"  -> 0.00,
         "sd"  -> 3.0
     )
    ),
    (61.0, // M1
     Map("V"   -> 4.67,
         "IV"  -> 3.33,
         "III" -> 1.41,
         "II"  -> 0.84,
         "Ib"  -> 0.51,
         "Iab" -> 0.19,
         "Ia"  -> -0.07
     )
    ),
    (62.0, // M2
     Map("V"   -> 4.69,
         "IV"  -> 3.31,
         "III" -> 1.31,
         "II"  -> 0.70,
         "Ib"  -> 0.39,
         "Iab" -> 0.09,
         "Ia"  -> -0.13
     )
    ),
    (63.0, // M3
     Map("V"   -> 4.71,
         "IV"  -> 3.28,
         "III" -> 1.12,
         "II"  -> 0.38,
         "Ib"  -> 0.10,
         "Iab" -> -0.16,
         "Ia"  -> -0.34
     )
    ),
    (69.0, // M9
     Map("V"   -> 5.32,
         "IV"  -> 3.15,
         "III" -> -0.10,
         "II"  -> -0.30,
         "Ib"  -> -0.50,
         "Iab" -> -0.60,
         "Ia"  -> -0.70
     )
    )
  )

  /**
   * Calculate surface gravity (log g) from stellar spectral classification.
   *
   * Uses interpolation from Straizys & Kuriliene, 1981, Ap&SS, 80, 353S gravity table.
   */
  def calculateGravity(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[Double] =
    (luminosity, temperature) match
      case (Nil, _) | (_, Nil) | (Nil, Nil)   =>
        none
      case ((h :: _), t) if h.startsWith("D") =>
        // Handle white dwarfs
        WhiteDwarfLogG.some
      case _                                  =>
        val gravities = for {
          lc  <- luminosity
          tc  <- temperature
          scc <- spectralClassCode(tc)
          if scc < BrownDwarfThreshold
        } yield
          // Normalize luminosity matches Python logic
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
            // Round to 3 decimals to match Python behavior
            BigDecimal(gravities.sum / gravities.length)
              .setScale(3, BigDecimal.RoundingMode.HALF_UP)
              .toDouble
              .some

  /**
   * Interpolate log(g) for a given spectral class code and luminosity class. Ported from the
   * original python code by Andy Stephens.
   *
   * Python's gravity table has sparse entries for some luminosity classes (e.g., sd only at anchor
   * points). This implementation filters to entries that have the requested class before
   * interpolating, matching Python behavior.
   */
  private def interpolateGravity(scc: Double, lumClass: String): Double = {
    // Filter to entries that have this luminosity class (matches Python's sparse table handling)
    val entriesWithLC = GravityTable.filter(_._2.contains(lumClass))

    // If no entries have this class, fall back to V
    val tableToUse =
      if entriesWithLC.nonEmpty then entriesWithLC
      else GravityTable.filter(_._2.contains("V"))

    if tableToUse.isEmpty then return DefaultLogG

    val ((sccLow, mapLow), (sccHigh, mapHigh)) =
      tableToUse.partition(_._1 <= scc) match
        case (lows, highs) if lows.nonEmpty && highs.nonEmpty =>
          (lows.last, highs.head)
        case (lows, _) if lows.nonEmpty                       =>
          (lows.last, lows.last) // Extrapolate from last
        case (_, highs) if highs.nonEmpty                     =>
          (highs.head, highs.head) // Extrapolate from first
        case _                                                =>
          return DefaultLogG

    val logGLow  = mapLow(lumClass)
    val logGHigh = mapHigh(lumClass)

    // Linear interpolation
    if sccLow == sccHigh then logGLow
    else
      val fraction = (scc - sccLow) / (sccHigh - sccLow)
      logGLow + fraction * (logGHigh - logGLow)
  }

  def calculateParameters(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[StellarParameters] =
    for
      temp <- calculateTemperature(luminosity, temperature)
      logG <- calculateGravity(luminosity, temperature)
    yield StellarParameters(temp.withUnit[Kelvin], logG)
