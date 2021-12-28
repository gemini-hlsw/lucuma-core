// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order._
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism
import lucuma.core.math.dimensional._

import scala.collection.immutable.SortedMap

// T = Brightness type (Integrated or Surface)
sealed trait SpectralDefinition[T] {

  /**
   * Convert units to `T0` brightness type.
   *
   * @tparam `T0`
   *   `Integrated` or `Surface`
   */
  def to[T0](implicit
    convBrightness: TagConverter[Brightness[T], Brightness[T0]],
    convLine:       TagConverter[LineFlux[T], LineFlux[T0]],
    convContinuum:  TagConverter[FluxDensityContinuum[T], FluxDensityContinuum[T0]]
  ): SpectralDefinition[T0] = this match {
    case a @ SpectralDefinition.BandNormalized(_, _) => a.to[T0]
    case a @ SpectralDefinition.EmissionLines(_, _)  => a.to[T0]
  }
}

object SpectralDefinition {

  final case class BandNormalized[T](
    sed:          UnnormalizedSED,
    brightnesses: SortedMap[Band, BandBrightness[T]]
  ) extends SpectralDefinition[T] {
    lazy val bands: List[Band] = brightnesses.keys.toList

    /**
     * Convert units to `T0` brightness type.
     *
     * @tparam `T0`
     *   `Integrated` or `Surface`
     */
    def to[T0](implicit
      conv: TagConverter[Brightness[T], Brightness[T0]]
    ): BandNormalized[T0] =
      BandNormalized(
        sed,
        brightnesses.map { case (band, brightness) => band -> brightness.to[T0] }
      )
  }

  object BandNormalized {
    implicit def eqBandNormalized[T](implicit bbeq: Eq[BandBrightness[T]]): Eq[BandNormalized[T]] =
      Eq.by(x => (x.sed, x.brightnesses))

    /** @group Optics */
    def sed[T]: Lens[BandNormalized[T], UnnormalizedSED] =
      Focus[BandNormalized[T]](_.sed)

    /** @group Optics */
    def brightnesses[T]: Lens[BandNormalized[T], SortedMap[Band, BandBrightness[T]]] =
      Focus[BandNormalized[T]](_.brightnesses)

    /** @group Optics */
    def brightnessesT[T]: Traversal[BandNormalized[T], BandBrightness[T]] =
      brightnesses.each

    /** @group Optics */
    def brightnessIn[T](b: Band): Traversal[BandNormalized[T], BandBrightness[T]] =
      brightnesses.filterIndex((a: Band) => a === b)
  }

  // TODO Check if BigDecimal [parse from/toString to] "5e-19".
  final case class EmissionLines[T](
    lines:                SortedMap[Wavelength, EmissionLine[T]],
    fluxDensityContinuum: Measure[PosBigDecimal] Of FluxDensityContinuum[T]
  ) extends SpectralDefinition[T] {
    lazy val wavelengths: List[Wavelength] = lines.keys.toList

    /**
     * Convert units to `T0` brightness type.
     *
     * @tparam `T0`
     *   `Integrated` or `Surface`
     */
    def to[T0](implicit
      convLine:      TagConverter[LineFlux[T], LineFlux[T0]],
      convContinuum: TagConverter[FluxDensityContinuum[T], FluxDensityContinuum[T0]]
    ): EmissionLines[T0] =
      EmissionLines(
        lines.map { case (wavelength, line) => wavelength -> line.to[T0] },
        fluxDensityContinuum.toTag[FluxDensityContinuum[T0]]
      )
  }

  object EmissionLines {
    implicit def eqEmissionLineSpectralDefinition[T]: Eq[EmissionLines[T]] =
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
    def fluxDensityContinuum[T]: Lens[
      EmissionLines[T],
      Measure[PosBigDecimal] Of FluxDensityContinuum[T]
    ] = Focus[EmissionLines[T]](_.fluxDensityContinuum)
  }

  implicit def eqSpectralDefinition[T](implicit
    tbeq: Eq[BandBrightness[T]]
  ): Eq[SpectralDefinition[T]] = Eq.instance {
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
  def unnormalizedSED[T]: Optional[SpectralDefinition[T], UnnormalizedSED] =
    bandNormalized.andThen(BandNormalized.sed[T])

  /** @group Optics */
  def bandBrightnesses[T]: Optional[SpectralDefinition[T], SortedMap[Band, BandBrightness[T]]] =
    bandNormalized.andThen(BandNormalized.brightnesses[T])

  /** @group Optics */
  def bandBrightnessesT[T]: Traversal[SpectralDefinition[T], BandBrightness[T]] =
    bandNormalized.andThen(BandNormalized.brightnessesT[T])

  /** @group Optics */
  def bandBrightnessIn[T](b: Band): Traversal[SpectralDefinition[T], BandBrightness[T]] =
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
  def fluxDensityContinuum[T]: Optional[
    SpectralDefinition[T],
    Measure[PosBigDecimal] Of FluxDensityContinuum[T]
  ] = emissionLines.andThen(EmissionLines.fluxDensityContinuum[T])
}
