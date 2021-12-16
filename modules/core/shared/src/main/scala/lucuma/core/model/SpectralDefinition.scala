// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.GroupedUnitQty
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.Traversal
import monocle.macros.GenPrism

import scala.collection.immutable.SortedMap

// T = Brightness type (integral or surface)
sealed trait SpectralDefinition[T]

object SpectralDefinition {

  final case class BandNormalized[T](
    sed:          UnnormalizedSpectralEnergyDistribution,
    brightnesses: SortedMap[Band, BandBrightness[T]]
  ) extends SpectralDefinition[T] {
    lazy val bands: List[Band] = brightnesses.keys.toList
  }

  object BandNormalized {
    implicit def eqBandNormalized[T](implicit
      tbeq: Eq[BandBrightness[T]]
    ): Eq[BandNormalized[T]] =
      Eq.by(x => (x.sed, x.brightnesses))

    /** @group Optics */
    def sed[T]: Lens[BandNormalized[T], UnnormalizedSpectralEnergyDistribution] =
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
    fluxDensityContinuum: GroupedUnitQty[PosBigDecimal, FluxDensityContinuum[T]]
  ) extends SpectralDefinition[T] {
    lazy val wavelengths: List[Wavelength] = lines.keys.toList
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
    def fluxDensityContinuum[T]: Lens[
      EmissionLines[T],
      GroupedUnitQty[PosBigDecimal, FluxDensityContinuum[T]]
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
}
