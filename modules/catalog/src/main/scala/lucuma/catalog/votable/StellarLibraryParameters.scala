// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import cats.syntax.all.*
import lucuma.catalog.votable.StellarPhysics.StellarParameters
import lucuma.core.enums.StellarLibrarySpectrum

object StellarLibraryParameters:

  private val DefaultWhiteDwarfTemp: Int    = 16800
  private val DefaultWhiteDwarfLogG: Double = 8.0

  private val DefaultSubdwarfTemp: Int    = 5199
  private val DefaultSubdwarfLogG: Double = 4.0

  private val WhiteDwarfTwoDigitPattern = """^(D[A-Z]*)(\d)(\d)(.*)$""".r

  private def normalizeLibraryTag(tag: String): String =
    val stripped = tag.takeWhile(c => c != '_')
    stripped match
      case WhiteDwarfTwoDigitPattern(prefix, d1, d2, rest) =>
        s"$prefix$d1.$d2$rest"
      case _                                               => stripped

  private def parseLibraryTag(tag: String): (List[String], List[String]) =
    val normalized = normalizeLibraryTag(tag)
    SpectralTypeParsers.spectralType.parseAll(normalized) match
      case Right((lumClasses, tempClasses)) => (lumClasses, tempClasses)
      case Left(_)                          => (List.empty, List.empty)

  /**
   * Map of stellar library spectra to their physical params. Lazy initialization to calculate
   * params on first access.
   */
  lazy val params: Map[StellarLibrarySpectrum, StellarParameters] =
    StellarLibrarySpectrum.values.flatMap { spectrum =>
      val (lumClasses, tempClasses) = parseLibraryTag(spectrum.tag)

      val params = (spectrum.tag, lumClasses, tempClasses) match
        // White dwarfs without temperature number
        case (tag, lc :: _, Nil) if tag.startsWith("D") =>
          Some(StellarParameters(DefaultWhiteDwarfTemp.withUnit[Kelvin], DefaultWhiteDwarfLogG))

        // Subdwarf
        case ("sd", _, _) =>
          Some(StellarParameters(DefaultSubdwarfTemp.withUnit[Kelvin], DefaultSubdwarfLogG))

        // Standard
        case (_, lc, tc) if lc.nonEmpty && tc.nonEmpty =>
          StellarPhysics.calculateParameters(lc, tc)

        case _ =>
          none

      params.tupleLeft(spectrum)
    }.toMap
