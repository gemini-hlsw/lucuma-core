// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.parse.Parser.char
import cats.parse.Rfc5234.digit
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin

/**
 * Stellar physics calculations for matching spectral types to library SEDs. Based on the Python
 * reference implementation (match_sed.py by Andrew Stephens).
 *
 * References:
 *   - Temperature: Malkov et al, 2020, RAA, 20, 139
 *   - Gravity: Straizys & Kuriliene, 1981, Ap&SS, 80, 353S
 */
object StellarPhysics:

  case class StellarParameters(
    tEff: Quantity[Int, Kelvin],     // Effective temperature
    logG: Double                      // Log of surface gravity (dimensionless)
  )

  /**
   * Convert a spectral class string to a spectral class code. O0 = 0, B0 = 10, A0 = 20, F0 = 30, G0 =
   * 40, K0 = 50, M0 = 60, etc. Based on Malkov et al, 2020, RAA, 20, 139, Figure 1.
   */
  def spectralClassCode(spectralClass: String): Option[Double] =
    if spectralClass.isEmpty then return None

    val letterConversions = Map(
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

    val letter = spectralClass.head
    letterConversions.get(letter).flatMap { baseCode =>
      // Reuse parser from SpectralTypeParsers: digit optionally followed by decimal
      val subclassParser = (digit ~ (char('.') ~ digit.rep).?).string

      val subclass = subclassParser.parse(spectralClass.tail) match
        case Right((_, value)) =>
          value.toDoubleOption.getOrElse(5.0)
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
   * Calculate effective temperature from luminosity and temperature classes. Uses the polynomial
   * relation from Malkov et al, 2020, RAA, 20, 139. This is the relation for giants (III) but works
   * well for all luminosity classes I-V.
   */
  def calculateTemperature(
    luminosityClasses:  List[String],
    temperatureClasses: List[String]
  ): Option[Int] =
    if luminosityClasses.isEmpty || temperatureClasses.isEmpty then return None

    // Handle white dwarfs: T_eff = 50400 / number
    if luminosityClasses.head.startsWith("D") then
      return temperatureClasses.headOption.flatMap { tc =>
        tc.toDoubleOption.map(num => (50400.0 / num).round.toInt)
      }

    // Calculate temperature for each temperature class
    val temperatures = temperatureClasses.flatMap { tc =>
      spectralClassCode(tc).flatMap { scc =>
        if scc >= 70.0 then None // No support for brown dwarfs (L, T, Y) yet
        else
          // Polynomial from Malkov et al. 2020
          val logTEff =
            5.07073 - 7.57056e-2 * scc + 1.47089e-3 * scc * scc - 1.03905e-5 * scc * scc * scc
          Some(math.pow(10, logTEff).round.toInt)
      }
    }

    if temperatures.isEmpty then None
    else Some((temperatures.sum.toDouble / temperatures.length).round.toInt)

  /**
   * Gravity interpolation table from Straizys & Kuriliene, 1981, Ap&SS, 80, 353S. Extended with
   * data from Malkov et al, 2020, RAA, 20, 139.
   */
  private val gravityTable: List[(Double, Map[String, Double])] = List(
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
   * Calculate log(g) from luminosity and temperature classes. Uses interpolation from Straizys &
   * Kuriliene, 1981, Ap&SS, 80, 353S.
   */
  def calculateGravity(
    luminosityClasses:  List[String],
    temperatureClasses: List[String]
  ): Option[Double] =
    if luminosityClasses.isEmpty || temperatureClasses.isEmpty then return None

    // Handle white dwarfs
    if luminosityClasses.head.startsWith("D") then return Some(8.0)

    val allLogG = for
      lc  <- luminosityClasses
      tc  <- temperatureClasses
      scc <- spectralClassCode(tc)
      if scc < 70.0 // No brown dwarf support
    yield
      // Normalize luminosity class - matches Python logic
      val normalizedLC = lc match
        case "I"                                                                              => "Iab" // Default I to Iab
        case "VI"                                                                             => "sd"  // VI is subdwarf
        case s if !Set("Ia", "Ib", "Iab").contains(s) && (s.endsWith("a") || s.endsWith("b")) =>
          s.dropRight(1) // Drop subclass for non Ia/Ib/Iab
        case s                                                                                => s

      interpolateGravity(scc, normalizedLC)

    if allLogG.isEmpty then None
    else
      Some(
        BigDecimal(allLogG.sum / allLogG.length)
          .setScale(3, BigDecimal.RoundingMode.HALF_UP)
          .toDouble
      )

  /**
   * Interpolate log(g) for a given spectral class code and luminosity class.
   */
  private def interpolateGravity(scc: Double, lumClass: String): Double =
    // Find the two table entries to interpolate between
    val (lower, upper) = gravityTable.partition(_._1 <= scc) match
      case (lows, highs) if lows.nonEmpty && highs.nonEmpty =>
        (lows.last, highs.head)
      case (lows, _) if lows.nonEmpty                       =>
        (lows.last, lows.last) // Extrapolate from last entry
      case (_, highs) if highs.nonEmpty                     =>
        (highs.head, highs.head) // Extrapolate from first entry
      case _                                                =>
        return 4.0 // Default

    val (sccLow, mapLow)   = lower
    val (sccHigh, mapHigh) = upper

    // Get gravity values for this luminosity class
    val logGLow  = mapLow.getOrElse(lumClass, mapLow.getOrElse("V", 4.0))
    val logGHigh = mapHigh.getOrElse(lumClass, mapHigh.getOrElse("V", 4.0))

    // Linear interpolation
    if sccLow == sccHigh then logGLow
    else
      val fraction = (scc - sccLow) / (sccHigh - sccLow)
      logGLow + fraction * (logGHigh - logGLow)

  /**
   * Calculate stellar parameters (T_eff and log_g) from spectral classification.
   */
  def calculateParameters(
    luminosityClasses:  List[String],
    temperatureClasses: List[String]
  ): Option[StellarParameters] =
    for
      tEff <- calculateTemperature(luminosityClasses, temperatureClasses)
      logG <- calculateGravity(luminosityClasses, temperatureClasses)
    yield StellarParameters(tEff.withUnit[Kelvin], logG)
