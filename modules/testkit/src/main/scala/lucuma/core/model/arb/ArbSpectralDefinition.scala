// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.Order._
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.model.BandBrightness
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSpectralEnergyDistribution
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbSpectralDefinition {
  import ArbUnnormalizedSpectralEnergyDistribution._
  import ArbBandBrightness._
  import ArbEnumerated._
  import SpectralDefinition._
  import BrightnessUnits._
  import ArbQty._
  import ArbEmissionLine._
  import ArbRefined._
  import ArbWavelength._

  implicit def arbBrightnessesMap[T](implicit
    arbUnit: Arbitrary[GroupedUnitType[Brightness[T]]]
  ): Arbitrary[SortedMap[Band, BandBrightness[T]]] =
    Arbitrary(
      arbitrary[Vector[BandBrightness[T]]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*))
    )

  implicit def cogBrightnessesMap[T](implicit
    cogenUnit: Cogen[GroupedUnitType[Brightness[T]]]
  ): Cogen[SortedMap[Band, BandBrightness[T]]] =
    Cogen[Vector[(Band, BandBrightness[T])]].contramap(_.toVector)

  implicit def arbEmissionLineMap[T](implicit
    arbUnit: Arbitrary[GroupedUnitType[LineFlux[T]]]
  ): Arbitrary[SortedMap[Wavelength, EmissionLine[T]]] =
    Arbitrary(
      arbitrary[Vector[EmissionLine[T]]]
        .map(_.fproductLeft(_.wavelength))
        .map(x => SortedMap(x: _*))
    )

  implicit def cogEmissionLineMap[T](implicit
    cogenUnit: Cogen[GroupedUnitType[LineFlux[T]]]
  ): Cogen[SortedMap[Wavelength, EmissionLine[T]]] =
    Cogen[Vector[(Wavelength, EmissionLine[T])]].contramap(_.toVector)

  implicit def arbBandNormalizedSpectralDefinition[T](implicit
    arbUnit: Arbitrary[GroupedUnitType[Brightness[T]]]
  ): Arbitrary[BandNormalized[T]] =
    Arbitrary {
      for {
        s <- arbitrary[UnnormalizedSpectralEnergyDistribution]
        b <- arbitrary[SortedMap[Band, BandBrightness[T]]]
      } yield BandNormalized(s, b)
    }

  implicit def cogBandNormalizedSpectralDefinition[T](implicit
    cogenUnit: Cogen[GroupedUnitType[Brightness[T]]]
  ): Cogen[BandNormalized[T]] =
    Cogen[(UnnormalizedSpectralEnergyDistribution, SortedMap[Band, BandBrightness[T]])].contramap(
      x => (x.sed, x.brightnesses)
    )

  implicit def arbEmissionLines[T](implicit
    arbLineFluxUnit:             Arbitrary[GroupedUnitType[LineFlux[T]]],
    arbFluxDensityContinuumUnit: Arbitrary[GroupedUnitType[FluxDensityContinuum[T]]]
  ): Arbitrary[EmissionLines[T]] =
    Arbitrary {
      for {
        l <- arbitrary[SortedMap[Wavelength, EmissionLine[T]]]
        c <- arbitrary[GroupedUnitQty[PosBigDecimal, FluxDensityContinuum[T]]]
      } yield EmissionLines[T](l, c)
    }

  implicit def cogEmissionLines[T](implicit
    cogenLineFluxUnit:             Cogen[GroupedUnitType[LineFlux[T]]],
    cogenFluxDensityContinuumUnit: Cogen[GroupedUnitType[FluxDensityContinuum[T]]]
  ): Cogen[EmissionLines[T]] =
    Cogen[
      (
        SortedMap[Wavelength, EmissionLine[T]],
        GroupedUnitQty[PosBigDecimal, FluxDensityContinuum[T]]
      )
    ].contramap(x => (x.lines, x.fluxDensityContinuum))

  implicit def arbSpectralDefinition[T](implicit
    arbBrightnessUnit: Arbitrary[GroupedUnitType[Brightness[T]]],
    arbLineUnit:       Arbitrary[GroupedUnitType[LineFlux[T]]],
    arbContinuumUnit:  Arbitrary[GroupedUnitType[FluxDensityContinuum[T]]]
  ): Arbitrary[SpectralDefinition[T]] =
    Arbitrary(
      Gen.oneOf(arbitrary[BandNormalized[T]], arbitrary[EmissionLines[T]])
    )

  implicit def cogSpectralDefinition[T](implicit
    cogenBrightnessUnit: Cogen[GroupedUnitType[Brightness[T]]],
    cogenLineUnit:       Cogen[GroupedUnitType[LineFlux[T]]],
    cogenContinuumUnit:  Cogen[GroupedUnitType[FluxDensityContinuum[T]]]
  ): Cogen[SpectralDefinition[T]] =
    Cogen[Either[BandNormalized[T], EmissionLines[T]]].contramap {
      case d @ BandNormalized(_, _) => d.asLeft
      case d @ EmissionLines(_, _)  => d.asRight
    }
}

object ArbSpectralDefinition extends ArbSpectralDefinition
