// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import lucuma.catalog.votable.StellarPhysics.StellarParameters
import lucuma.core.enums.StellarLibrarySpectrum

/**
 * Pre-calculated physical parameters for each stellar library SED.
 * Based on parsing spectral types from tag fields and calculating T_eff and log_g.
 */
object StellarLibraryParameters:

  /**
   * Extract luminosity and temperature classes from a library spectrum tag.
   * E.g., "G2V" -> (["V"], ["G2"])
   *       "K1.5III" -> (["III"], ["K1.5"])
   */
  private def parseLibraryTag(tag: String): (List[String], List[String]) =
    // Handle white dwarfs (DA, DB, etc.)
    if tag.startsWith("D") then
      val lumClass = tag.takeWhile(c => c.isLetter || c == 'A' || c == 'B' || c == 'Q' || c == 'P' || c == 'Z')
      val tempClass = tag.drop(lumClass.length)
      return (List(lumClass), List(tempClass))

    // Handle subdwarfs
    if tag == "sd" then
      return (List("sd"), List.empty)

    // Standard spectral types: extract Roman numerals for luminosity
    val romanPattern = """(VIII|VII|VI|IV|III|II|I|V)[ab]?""".r
    val lumClasses = romanPattern.findAllIn(tag).toList

    // Extract temperature class (letter + number)
    val tempPattern = """[OBAFGKMLTY][0-9]?\.?[0-9]?[\+-]?""".r
    val tempClasses = tempPattern.findAllIn(tag).toList

    (lumClasses, tempClasses)

  /**
   * Map of stellar library spectra to their physical parameters.
   * Lazy initialization to calculate parameters on first access.
   */
  lazy val parameters: Map[StellarLibrarySpectrum, StellarParameters] =
    StellarLibrarySpectrum.values.flatMap { spectrum =>
      val (lumClasses, tempClasses) = parseLibraryTag(spectrum.tag)

      // Special cases that need manual override
      val params = (spectrum.tag, lumClasses, tempClasses) match
        // White dwarfs without temperature number default to mid-range
        case (tag, lc :: _, Nil) if tag.startsWith("D") =>
          Some(StellarParameters(16800, 8.0)) // ~DA3 equivalent

        // Subdwarf without specific type
        case ("sd", _, _) =>
          Some(StellarParameters(5199, 4.0)) // Generic subdwarf

        // Standard stars
        case (_, lc, tc) if lc.nonEmpty && tc.nonEmpty =>
          StellarPhysics.calculateParameters(lc, tc)

        // Fallback for malformed tags
        case _ =>
          None

      params.map(spectrum -> _)
    }.toMap

  /**
   * Get physical parameters for a library spectrum.
   */
  def getParameters(spectrum: StellarLibrarySpectrum): Option[StellarParameters] =
    parameters.get(spectrum)
