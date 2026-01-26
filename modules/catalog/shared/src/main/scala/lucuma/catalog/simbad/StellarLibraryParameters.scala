// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.syntax.all.*
import lucuma.catalog.simbad.StellarPhysics.StellarParameters
import lucuma.core.enums.StellarLibrarySpectrum

private[simbad] class StellarLibraryParameters(
  config:  StellarLibraryConfig,
  physics: StellarPhysics
):

  private val librarySpectraOrdered: List[(StellarLibrarySpectrum, (List[String], List[String]))] =
    config.entries

  private val librarySpectraConfig: Map[StellarLibrarySpectrum, (List[String], List[String])] =
    librarySpectraOrdered.toMap

  val preferredSpectraOrdered: List[StellarLibrarySpectrum] =
    librarySpectraOrdered.map(_._1)

  val preferredSpectra: Set[StellarLibrarySpectrum] = librarySpectraConfig.keySet

  def fileOrderIndex(spectrum: StellarLibrarySpectrum): Int =
    preferredSpectraOrdered.indexOf(spectrum) match
      case -1 => Int.MaxValue
      case i  => i

  def getLuminosityClasses(spectrum: StellarLibrarySpectrum): List[String] =
    librarySpectraConfig.get(spectrum).map(_._1).getOrElse(List.empty)

  lazy val params: Map[StellarLibrarySpectrum, StellarParameters] =
    librarySpectraConfig.flatMap { case (spectrum, (lumClasses, tempClasses)) =>
      physics.calculateParameters(lumClasses, tempClasses).tupleLeft(spectrum)
    }
