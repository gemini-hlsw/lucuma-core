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

  private val LetterConversions = Map(
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
    tEff: Quantity[Int, Kelvin], // Effective temperature
    logG: Double                 // Log of surface gravity (dimensionless)
  )

  /**
   * Convert a spectral class string to a numerical code.
   *
   * Maps spectral type letters to base values (O=0, B=10, A=20, ..., Y=90) and adds subclass digits
   * (0-9.5).
   *
   * Examples:
   *   - "O0" → 0.0
   *   - "G2" → 42.0
   *   - "G2.7" → 42.7
   *   - "G2+" → 42.25
   *   - "K6-" → 55.75
   *   - "G" (bare letter) → 45.0 (defaults to subclass 5)
   *
   * Based on Malkov and match_sed.py
   */
  def spectralClassCode(spectralClass: String): Option[Double] =
    if spectralClass.isEmpty then return None

    val letter = spectralClass.head
    LetterConversions.get(letter).flatMap { baseCode =>
      val subclass = SpectralTypeParsers.tempSubclass.parse(spectralClass.tail) match
        case Right((_, value)) =>
          value.parseDoubleOption.getOrElse(5.0)
        case Left(_)           =>
          5.0 // Default to 5 if no subclass specified

      // Handle +/- modifiers (quarter subclass)
      val modifier =
        if spectralClass.contains('+') then 0.25
        else if spectralClass.contains('-') then -0.25
        else 0.0

      Some(baseCode + subclass + modifier)
    }

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
      case (Nil, _) | (_, Nil) | (Nil, Nil)   => none
      case ((h :: _), t) if h.startsWith("D") =>
        // white dwarfs: T_eff = 50400 / number
        t.headOption.flatMap: tc =>
          tc.parseDoubleOption.map(num => (50400.0 / num).round.toInt)
      case _                                  =>
        // Calculate temperature for each temperature class
        temperature
          .flatMap: tc =>
            spectralClassCode(tc).flatMap:
              // No support for brown dwarfs (L, T, Y) yet
              case scc if scc >= 70.0 => None
              case scc                =>
                // Polynomial from Malkov et al. 2020
                val logTEff =
                  5.07073 - 7.57056e-2 * scc + 1.47089e-3 * scc * scc - 1.03905e-5 * scc * scc * scc
                math.pow(10, logTEff).round.toInt.some
        match
          case Nil => none
          // Return average
          case t   => (t.sum.toDouble / t.length).round.toInt.some

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
    (5.0,
     Map("V"   -> 3.90,
         "IV"  -> 3.86,
         "III" -> 3.82,
         "II"  -> 3.76,
         "Ib"  -> 3.74,
         "Iab" -> 3.69,
         "Ia"  -> 3.57
     )
    ),
    (10.0,
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
    (20.0,
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
    (30.0,
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
    (40.0,
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
    (50.0,
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
    (60.0,
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
    (69.0,
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
   * Uses interpolation from Straizys & Kuriliene, 1981, Ap&SS, 80, 353S gravity table. Normalizes
   * luminosity classes (I→Iab, VI→sd, drops subclasses from non-supergiant types). White dwarfs use
   * fixed log g = 8.0.
   */
  def calculateGravity(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[Double] =
    (luminosity, temperature) match
      case (Nil, _) | (_, Nil) | (Nil, Nil)   => none
      case ((h :: _), t) if h.startsWith("D") =>
        // Handle white dwarfs
        8.0.some
      case _                                  =>
        val allLogG = for {
          lc  <- luminosity
          tc  <- temperature
          scc <- spectralClassCode(tc)
          if scc < 70.0 // No brown dwarf support
        } yield
          // Normalize luminosity matches Python logic
          val supergiants  = Set("Ia", "Ib", "Iab")
          val normalizedLC = lc match
            case "I"                                     => "Iab"
            case "VI"                                    => "sd"
            case s if supergiants.contains(s)            => s
            case s if s.endsWith("a") || s.endsWith("b") => s.dropRight(1)
            case s                                       => s

          interpolateGravity(scc, normalizedLC)

        allLogG match
          case Nil => none
          case _   =>
            // adjust to 3 decimals
            BigDecimal(allLogG.sum / allLogG.length)
              .setScale(3, BigDecimal.RoundingMode.HALF_UP)
              .toDouble
              .some

  /**
   * Interpolate log(g) for a given spectral class code and luminosity class. Ported from python
   */
  private def interpolateGravity(scc: Double, lumClass: String): Double =
    // Find the two table entries to interpolate between
    val ((sccLow, mapLow), (sccHigh, mapHigh)) =
      GravityTable.partition(_._1 <= scc) match
        case (lows, highs) if lows.nonEmpty && highs.nonEmpty =>
          (lows.last, highs.head)
        case (lows, _) if lows.nonEmpty                       =>
          (lows.last, lows.last) // Extrapolate from last entry
        case (_, highs) if highs.nonEmpty                     =>
          (highs.head, highs.head) // Extrapolate from first entry
        case _                                                =>
          return 4.0 // Default

    // Get gravity values for this luminosity class
    val logGLow  = mapLow.getOrElse(lumClass, mapLow.getOrElse("V", 4.0))
    val logGHigh = mapHigh.getOrElse(lumClass, mapHigh.getOrElse("V", 4.0))

    // Linear interpolation
    if sccLow == sccHigh then logGLow
    else
      val fraction = (scc - sccLow) / (sccHigh - sccLow)
      logGLow + fraction * (logGHigh - logGLow)

  def calculateParameters(
    luminosity:  List[String],
    temperature: List[String]
  ): Option[StellarParameters] =
    for
      tEff <- calculateTemperature(luminosity, temperature)
      logG <- calculateGravity(luminosity, temperature)
    yield StellarParameters(tEff.withUnit[Kelvin], logG)
