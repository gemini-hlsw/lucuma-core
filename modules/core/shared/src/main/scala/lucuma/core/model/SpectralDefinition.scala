// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism

import scala.collection.immutable.SortedMap

// T = Brightness type (Integrated or Surface)
sealed trait SpectralDefinition[T] {

  /**
   * Convert units to `T0` brightness type.
   *
   * @tparam `T0`
   *   `Integrated` or `Surface`
   */
  def to[T0](using
    TagConverter[Brightness[T], Brightness[T0]],
    TagConverter[LineFlux[T], LineFlux[T0]],
    TagConverter[FluxDensityContinuum[T], FluxDensityContinuum[T0]]
  ): SpectralDefinition[T0] = this match {
    case a @ SpectralDefinition.BandNormalized(_, _) => a.to[T0]
    case a @ SpectralDefinition.EmissionLines(_, _)  => a.to[T0]
  }
}

object SpectralDefinition {

  final case class BandNormalized[T](
    sed:          Option[UnnormalizedSED],
    brightnesses: SortedMap[Band, BrightnessMeasure[T]]
  ) extends SpectralDefinition[T] {
    lazy val bands: List[Band] = brightnesses.keys.toList

    /**
     * Convert units to `T0` brightness type.
     *
     * @tparam `T0`
     *   `Integrated` or `Surface`
     */
    def to[T0](using TagConverter[Brightness[T], Brightness[T0]]
    ): BandNormalized[T0] =
      BandNormalized(
        sed,
        brightnesses.map { case (band, brightness) => band -> brightness.toTag[Brightness[T0]] }
      )

    /**
     * Returns the defined band closest to the given wavelength.
     */
    def nearestBand(wavelength: Wavelength): Option[Band] =
      brightnesses.keySet.minByOption: band =>
        (wavelength.toPicometers.value.value - band.center.toPicometers.value.value).abs
  }

  object BandNormalized {
    given eqBandNormalized[T](using Eq[BrightnessMeasure[T]]): Eq[BandNormalized[T]] =
      Eq.by(x => (x.sed, x.brightnesses))

    /** @group Optics */
    def sed[T]: Lens[BandNormalized[T], Option[UnnormalizedSED]] =
      Focus[BandNormalized[T]](_.sed)

    /** @group Optics */
    def brightnesses[T]: Lens[BandNormalized[T], SortedMap[Band, BrightnessMeasure[T]]] =
      Focus[BandNormalized[T]](_.brightnesses)

    /** @group Optics */
    def brightnessesT[T]: Traversal[BandNormalized[T], BrightnessMeasure[T]] =
      brightnesses.each

    /** @group Optics */
    def brightnessIn[T](b: Band): Traversal[BandNormalized[T], BrightnessMeasure[T]] =
      brightnesses.filterIndex((a: Band) => a === b)
  }

  final case class EmissionLines[T](
    lines:                SortedMap[Wavelength, EmissionLine[T]],
    fluxDensityContinuum: FluxDensityContinuumMeasure[T]
  ) extends SpectralDefinition[T] {
    lazy val wavelengths: List[Wavelength] = lines.keys.toList

    /**
     * Convert units to `T0` brightness type.
     *
     * @tparam `T0`
     *   `Integrated` or `Surface`
     */
    def to[T0](using
      TagConverter[LineFlux[T], LineFlux[T0]],
      TagConverter[FluxDensityContinuum[T], FluxDensityContinuum[T0]]
    ): EmissionLines[T0] =
      EmissionLines(
        lines.map { case (wavelength, line) => wavelength -> line.to[T0] },
        fluxDensityContinuum.toTag[FluxDensityContinuum[T0]]
      )

    /**
     * Returns the defined line closest to the given wavelength.
     */
    def nearestLine(wavelength: Wavelength): Option[Wavelength] =
      lines.keySet.minByOption: line =>
        (wavelength.toPicometers.value.value - line.toPicometers.value.value).abs
  }

  object EmissionLines {
    given eqEmissionLineSpectralDefinition[T]: Eq[EmissionLines[T]] =
      Eq.by(x => (x.lines, x.fluxDensityContinuum))

    /** @group Optics */
    def lines[T]: Lens[EmissionLines[T], SortedMap[Wavelength, EmissionLine[T]]] =
      Focus[EmissionLines[T]](_.lines)

    /** @group Optics */
    def linesT[T]: Traversal[EmissionLines[T], EmissionLine[T]] =
      lines.each

    /** @group Optics */
    def lineIn[T](w: Wavelength): Traversal[EmissionLines[T], EmissionLine[T]] =
      lines.filterIndex((a: Wavelength) => a === w)

    /** @group Optics */
    def fluxDensityContinuum[T]: Lens[EmissionLines[T], FluxDensityContinuumMeasure[T]] =
      Focus[EmissionLines[T]](_.fluxDensityContinuum)
  }

  given eqSpectralDefinition[T](using Eq[BrightnessMeasure[T]]): Eq[SpectralDefinition[T]] =
    Eq.instance {
      case (a @ BandNormalized(_, _), b @ BandNormalized(_, _)) => a === b
      case (a @ EmissionLines(_, _), b @ EmissionLines(_, _))   => a === b
      case _                                                    => false
    }

  /** @group Optics */
  def bandNormalized[T]: Prism[SpectralDefinition[T], BandNormalized[T]] =
    GenPrism[SpectralDefinition[T], BandNormalized[T]]

  /** @group Optics */
  def emissionLines[T]: Prism[SpectralDefinition[T], EmissionLines[T]] =
    GenPrism[SpectralDefinition[T], EmissionLines[T]]

  /** @group Optics */
  def unnormalizedSED[T]: Optional[SpectralDefinition[T], Option[UnnormalizedSED]] =
    bandNormalized.andThen(BandNormalized.sed[T])

  /** @group Optics */
  def brightnesses[T]: Optional[SpectralDefinition[T], SortedMap[Band, BrightnessMeasure[T]]] =
    bandNormalized.andThen(BandNormalized.brightnesses[T])

  /** @group Optics */
  def brightnessesT[T]: Traversal[SpectralDefinition[T], BrightnessMeasure[T]] =
    bandNormalized.andThen(BandNormalized.brightnessesT[T])

  /** @group Optics */
  def brightnessIn[T](b: Band): Traversal[SpectralDefinition[T], BrightnessMeasure[T]] =
    bandNormalized.andThen(BandNormalized.brightnessIn[T](b))

  /** @group Optics */
  def wavelengthLines[T]: Optional[SpectralDefinition[T], SortedMap[Wavelength, EmissionLine[T]]] =
    emissionLines.andThen(EmissionLines.lines[T])

  /** @group Optics */
  def wavelengthLinesT[T]: Traversal[SpectralDefinition[T], EmissionLine[T]] =
    emissionLines.andThen(EmissionLines.linesT[T])

  /** @group Optics */
  def wavelengthLineIn[T](w: Wavelength): Traversal[SpectralDefinition[T], EmissionLine[T]] =
    emissionLines.andThen(EmissionLines.lineIn[T](w))

  /** @group Optics */
  def fluxDensityContinuum[T]: Optional[SpectralDefinition[T], FluxDensityContinuumMeasure[T]] =
    emissionLines.andThen(EmissionLines.fluxDensityContinuum[T])
}
