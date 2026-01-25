// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.syntax.all.*
import lucuma.catalog.votable.StellarPhysics.StellarParameters
import lucuma.core.enums.StellarLibrarySpectrum

object StellarLibraryParameters:

  import StellarLibrarySpectrum.*

  /**
   * Ordered list from Python's match_sed_stars.dat. The order matters for tie-breaking when
   * multiple spectra have identical scores. Python's sort is stable, so the first match in file
   * order wins on ties.
   */
  private val librarySpectraOrdered: List[(StellarLibrarySpectrum, (List[String], List[String]))] =
    List(
      // White dwarfs (in Python file order)
      DA08      -> (List("DA"), List("0.8")),
      DA09      -> (List("DA"), List("0.9")),
      DA12      -> (List("DA"), List("1.2")),
      DA15      -> (List("DA"), List("1.5")),
      DA18      -> (List("DA"), List("1.8")),
      DA24      -> (List("DA"), List("2.4")),
      DA28      -> (List("DA"), List("2.8")),
      DA30      -> (List("DA"), List("3.0")),
      DA31      -> (List("DA"), List("3.1")),
      DA33      -> (List("DA"), List("3.3")),
      DA36      -> (List("DA"), List("3.6")),
      DA38      -> (List("DA"), List("3.8")),
      DA48      -> (List("DA"), List("4.8")),
      DA57      -> (List("DA"), List("5.7")),
      DBQ40     -> (List("DB"), List("4.0")),
      DBQA50    -> (List("DBQA"), List("5.0")),
      DO20      -> (List("DO"), List("2.0")),
      // Supergiants (I)
      A0I       -> (List("I"), List("A0")),
      B5I       -> (List("I"), List("B5")),
      F0I_new   -> (List("I"), List("F0")),
      F5I_new   -> (List("I"), List("F5")),
      F8I       -> (List("I"), List("F8")),
      G0I_new   -> (List("I"), List("G0")),
      G2I       -> (List("I"), List("G2")),
      G5I_new   -> (List("I"), List("G5")),
      G8I       -> (List("I"), List("G8")),
      K2I       -> (List("I"), List("K2")),
      K4I_new   -> (List("I"), List("K4")),
      M2I       -> (List("I"), List("M2")),
      // Bright giants (II)
      F0II      -> (List("II"), List("F0")),
      F2II      -> (List("II"), List("F2")),
      K0_1II    -> (List("II"), List("K0.5")),
      K3II      -> (List("II"), List("K3")),
      // Giants (III)
      A0III_new -> (List("III"), List("A0")),
      A5III     -> (List("III"), List("A5")),
      A8III     -> (List("III"), List("A8")),
      B5III     -> (List("III"), List("B5")),
      B9III     -> (List("III"), List("B9")),
      F0III_new -> (List("III"), List("F0")),
      F2III     -> (List("III"), List("F2")),
      F5III_new -> (List("III"), List("F5")),
      G0III     -> (List("III"), List("G0")),
      G5III_new -> (List("III"), List("G5")),
      G7III     -> (List("III"), List("G7")),
      G8III     -> (List("III"), List("G8")),
      K0III_new -> (List("III"), List("K0")),
      K05III    -> (List("III"), List("K0.5")),
      K15III    -> (List("III"), List("K1.5")),
      K2III     -> (List("III"), List("K2")),
      K3III     -> (List("III"), List("K3")),
      K4III_new -> (List("III"), List("K4")),
      K5III     -> (List("III"), List("K5")),
      M0III_new -> (List("III"), List("M0")),
      M1III     -> (List("III"), List("M1")),
      M2III     -> (List("III"), List("M2")),
      M3III_new -> (List("III"), List("M3")),
      M4III     -> (List("III"), List("M4")),
      M6III_new -> (List("III"), List("M6")),
      M7III     -> (List("III"), List("M7")),
      M8III     -> (List("III"), List("M8")),
      M9III     -> (List("III"), List("M9")),
      O8III     -> (List("III"), List("O8")),
      // Subgiants (IV)
      F0IV      -> (List("IV"), List("F0")),
      F8IV      -> (List("IV"), List("F8")),
      G2IV      -> (List("IV"), List("G2")),
      K0IV      -> (List("IV"), List("K0")),
      // Main sequence (V)
      A0V_new   -> (List("V"), List("A0")),
      A1V       -> (List("V"), List("A1")),
      A2V       -> (List("V"), List("A2")),
      A3V       -> (List("V"), List("A3")),
      A4V       -> (List("V"), List("A4")),
      A5V_new   -> (List("V"), List("A5")),
      A6V       -> (List("V"), List("A6")),
      B0V       -> (List("V"), List("B0")),
      B05V      -> (List("V"), List("B0.5")),
      B3V       -> (List("V"), List("B3")),
      B5_7V     -> (List("V"), List("B6")),
      F0V_new   -> (List("V"), List("F0")),
      F2V       -> (List("V"), List("F2")),
      F4V       -> (List("V"), List("F4")),
      F5V_new   -> (List("V"), List("F5")),
      F6V_r     -> (List("V"), List("F6")),
      F7V       -> (List("V"), List("F7")),
      F8V       -> (List("V"), List("F8")),
      G0V_new   -> (List("V"), List("G0")),
      G1V       -> (List("V"), List("G1")),
      G2V_new   -> (List("V"), List("G2")),
      G3V       -> (List("V"), List("G3")),
      G5V_new   -> (List("V"), List("G5")),
      G8V       -> (List("V"), List("G8")),
      K0V_new   -> (List("V"), List("K0")),
      K2V       -> (List("V"), List("K2")),
      K3V       -> (List("V"), List("K3")),
      K4V       -> (List("V"), List("K4")),
      K5V       -> (List("V"), List("K5")),
      M0V_new   -> (List("V"), List("M0")),
      M1V       -> (List("V"), List("M1")),
      M2V       -> (List("V"), List("M2")),
      M3V_new   -> (List("V"), List("M3")),
      M4V       -> (List("V"), List("M4")),
      M5V       -> (List("V"), List("M5")),
      M6V       -> (List("V"), List("M6")),
      O5V       -> (List("V"), List("O5")),
      O9V       -> (List("V"), List("O9")),
      O95V      -> (List("V"), List("O9.5")),
      // Subdwarfs
      sdB       -> (List("sd"), List("B")),
      sdF8      -> (List("sd"), List("F8")),
      sdO       -> (List("sd"), List("O"))
    )

  private val librarySpectraConfig: Map[StellarLibrarySpectrum, (List[String], List[String])] =
    librarySpectraOrdered.toMap

  /**
   * Curated list of library spectra in Python file order (for tie-breaking).
   */
  val preferredSpectraOrdered: List[StellarLibrarySpectrum] =
    librarySpectraOrdered.map(_._1)

  /**
   * Curated set of library spectra matching Python's match_sed_stars.dat.
   */
  val preferredSpectra: Set[StellarLibrarySpectrum] = librarySpectraConfig.keySet

  /**
   * Get the file order index for a spectrum (used for tie-breaking).
   */
  def fileOrderIndex(spectrum: StellarLibrarySpectrum): Int =
    preferredSpectraOrdered.indexOf(spectrum) match
      case -1 => Int.MaxValue
      case i  => i

  /**
   * Get the luminosity classes for a library spectrum.
   */
  def getLuminosityClasses(spectrum: StellarLibrarySpectrum): List[String] =
    librarySpectraConfig.get(spectrum).map(_._1).getOrElse(List.empty)

  /**
   * Map of stellar library spectra to their physical params. Uses explicit (lc, tc) mappings from
   * Python's match_sed_stars.dat.
   */
  lazy val params: Map[StellarLibrarySpectrum, StellarParameters] =
    librarySpectraConfig.flatMap { case (spectrum, (lumClasses, tempClasses)) =>
      StellarPhysics.calculateParameters(lumClasses, tempClasses).tupleLeft(spectrum)
    }
