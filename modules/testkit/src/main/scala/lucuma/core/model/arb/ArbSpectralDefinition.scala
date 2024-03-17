// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.Order.*
import cats.syntax.all.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbBrightnessValue
import lucuma.core.math.arb.ArbFluxDensityContinuumValue
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.*
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

import scala.collection.immutable.SortedMap

trait ArbSpectralDefinition {
  import ArbBrightnessValue.given
  import ArbEmissionLine.given
  import ArbEnumerated.given
  import ArbFluxDensityContinuumValue.given
  import ArbMeasure.given
  import ArbUnnormalizedSED.given
  import ArbWavelength.*
  import BrightnessUnits.*
  import SpectralDefinition.*

  given arbBandNormalizedSpectralDefinition[T](using
    arbUnit: Arbitrary[Units Of Brightness[T]]
  ): Arbitrary[BandNormalized[T]] =
    Arbitrary {
      for {
        s <- arbitrary[Option[UnnormalizedSED]]
        b <- arbitrary[SortedMap[Band, BrightnessMeasure[T]]]
      } yield BandNormalized(s, b)
    }

  given cogBandNormalizedSpectralDefinition[T]: Cogen[BandNormalized[T]] =
    Cogen[(Option[UnnormalizedSED], Map[Band, BrightnessMeasure[T]])].contramap(x =>
      (x.sed, x.brightnesses)
    )

  given arbEmissionLines[T](using
    arbLineFluxUnit:             Arbitrary[Units Of LineFlux[T]],
    arbFluxDensityContinuumUnit: Arbitrary[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[EmissionLines[T]] =
    Arbitrary {
      for {
        l <- arbitrary[SortedMap[Wavelength, EmissionLine[T]]]
        c <- arbitrary[FluxDensityContinuumMeasure[T]]
      } yield EmissionLines[T](l, c)
    }

  given cogEmissionLines[T]: Cogen[EmissionLines[T]] =
    Cogen[(Map[Wavelength, EmissionLine[T]], Measure[FluxDensityContinuumValue])].contramap(x =>
      (x.lines, x.fluxDensityContinuum)
    )

  given arbSpectralDefinition[T](using
    arbBrightnessUnit: Arbitrary[Units Of Brightness[T]],
    arbLineUnit:       Arbitrary[Units Of LineFlux[T]],
    arbContinuumUnit:  Arbitrary[Units Of FluxDensityContinuum[T]]
  ): Arbitrary[SpectralDefinition[T]] =
    Arbitrary(
      Gen.oneOf(arbitrary[BandNormalized[T]], arbitrary[EmissionLines[T]])
    )

  given cogSpectralDefinition[T]: Cogen[SpectralDefinition[T]] =
    Cogen[Either[BandNormalized[T], EmissionLines[T]]].contramap {
      case d @ BandNormalized(_, _) => d.asLeft
      case d @ EmissionLines(_, _)  => d.asRight
    }
}

object ArbSpectralDefinition extends ArbSpectralDefinition
