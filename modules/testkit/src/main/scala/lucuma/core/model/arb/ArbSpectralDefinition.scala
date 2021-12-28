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
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.model.BandBrightness
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbSpectralDefinition {
  import ArbUnnormalizedSED._
  import ArbBandBrightness._
  import ArbEnumerated._
  import SpectralDefinition._
  import BrightnessUnits._
  import ArbMeasure._
  import ArbEmissionLine._
  import ArbRefined._
  import ArbWavelength._

  implicit def arbBrightnessesMap[T](implicit
    arbUnit: Arbitrary[Units Of Brightness[T]]
  ): Arbitrary[SortedMap[Band, BandBrightness[T]]] =
    Arbitrary(
      arbitrary[Vector[BandBrightness[T]]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*))
    )

  implicit def cogBrightnessesMap[T]: Cogen[SortedMap[Band, BandBrightness[T]]] =
    Cogen[Vector[(Band, BandBrightness[T])]].contramap(_.toVector)

  implicit def arbEmissionLineMap[T](implicit
    arbUnit: Arbitrary[Units Of LineFlux[T]]
  ): Arbitrary[SortedMap[Wavelength, EmissionLine[T]]] =
    Arbitrary(
      arbitrary[Vector[EmissionLine[T]]]
        .map(_.fproductLeft(_.wavelength))
        .map(x => SortedMap(x: _*))
    )

  implicit def cogEmissionLineMap[T]: Cogen[SortedMap[Wavelength, EmissionLine[T]]] =
    Cogen[Vector[(Wavelength, EmissionLine[T])]].contramap(_.toVector)

  implicit def arbBandNormalizedSpectralDefinition[T](implicit
    arbUnit: Arbitrary[Units Of Brightness[T]]
  ): Arbitrary[BandNormalized[T]] =
    Arbitrary {
      for {
        s <- arbitrary[UnnormalizedSED]
        b <- arbitrary[SortedMap[Band, BandBrightness[T]]]
      } yield BandNormalized(s, b)
    }

  implicit def cogBandNormalizedSpectralDefinition[T]: Cogen[BandNormalized[T]] =
    Cogen[(UnnormalizedSED, SortedMap[Band, BandBrightness[T]])].contramap(x =>
      (x.sed, x.brightnesses)
    )

  implicit def arbEmissionLines[T](implicit
    arbLineFluxUnit:             Arbitrary[Units Of LineFlux[T]],
    arbFluxDensityContinuumUnit: Arbitrary[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[EmissionLines[T]] =
    Arbitrary {
      for {
        l <- arbitrary[SortedMap[Wavelength, EmissionLine[T]]]
        c <- arbitrary[Measure[PosBigDecimal] Of FluxDensityContinuum[T]]
      } yield EmissionLines[T](l, c)
    }

  implicit def cogEmissionLines[T]: Cogen[EmissionLines[T]] =
    Cogen[(SortedMap[Wavelength, EmissionLine[T]], Measure[PosBigDecimal])].contramap(x =>
      (x.lines, x.fluxDensityContinuum)
    )

  implicit def arbSpectralDefinition[T](implicit
    arbBrightnessUnit: Arbitrary[Units Of Brightness[T]],
    arbLineUnit:       Arbitrary[Units Of LineFlux[T]],
    arbContinuumUnit:  Arbitrary[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[SpectralDefinition[T]] =
    Arbitrary(
      Gen.oneOf(arbitrary[BandNormalized[T]], arbitrary[EmissionLines[T]])
    )

  implicit def cogSpectralDefinition[T]: Cogen[SpectralDefinition[T]] =
    Cogen[Either[BandNormalized[T], EmissionLines[T]]].contramap {
      case d @ BandNormalized(_, _) => d.asLeft
      case d @ EmissionLines(_, _)  => d.asRight
    }
}

object ArbSpectralDefinition extends ArbSpectralDefinition
