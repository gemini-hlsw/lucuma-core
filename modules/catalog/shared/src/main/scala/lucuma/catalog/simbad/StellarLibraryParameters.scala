// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.syntax.all.*
import lucuma.catalog.simbad.StellarPhysics.StellarParameters
import lucuma.core.enums.StellarLibrarySpectrum

private[catalog] class StellarLibraryParameters(
  config:  StellarLibraryConfig,
  physics: StellarPhysics
):

  private val librarySpectraConfig: Map[StellarLibrarySpectrum, (List[String], List[String])] =
    config.entries.toMap

  val preferredSpectraOrdered: List[StellarLibrarySpectrum] =
    config.entries.map(_._1)

  private val spectrumIndexMap: Map[StellarLibrarySpectrum, Int] =
    preferredSpectraOrdered.zipWithIndex.toMap

  def fileOrderIndex(spectrum: StellarLibrarySpectrum): Int =
    spectrumIndexMap.getOrElse(spectrum, Int.MaxValue)

  def luminosityClasses(spectrum: StellarLibrarySpectrum): List[String] =
    librarySpectraConfig.get(spectrum).foldMap(_._1)

  val params: Map[StellarLibrarySpectrum, StellarParameters] =
    librarySpectraConfig.flatMap:
      case (spectrum, (lumClasses, tempClasses)) =>
        physics.calculateParameters(lumClasses, tempClasses).tupleLeft(spectrum)
