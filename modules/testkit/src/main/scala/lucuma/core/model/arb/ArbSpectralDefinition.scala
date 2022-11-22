// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.Order.*
import cats.data.NonEmptyMap
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import org.typelevel.cats.time.instantInstances
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

import scala.collection.immutable.SortedMap

trait ArbSpectralDefinition {
  import ArbUnnormalizedSED.*
  import ArbEnumerated.*
  import SpectralDefinition.*
  import BrightnessUnits.*
  import ArbMeasure.*
  import ArbEmissionLine.*
  import ArbRefined.*
  import ArbWavelength.*

  implicit def cogNonEmptyMap[K: Cogen: Ordering, V: Cogen]: Cogen[NonEmptyMap[K, V]] =
    Cogen[Map[K, V]].contramap(_.toSortedMap.toMap)

  implicit def arbBandNormalizedSpectralDefinition[T](implicit
    arbUnit: Arbitrary[Units Of Brightness[T]]
  ): Arbitrary[BandNormalized[T]] =
    Arbitrary {
      for {
        s <- arbitrary[UnnormalizedSED]
        b <- arbitrary[SortedMap[Band, BrightnessMeasureOverTime[T]]]
      } yield BandNormalized(s, b)
    }

  implicit def cogBandNormalizedSpectralDefinition[T]: Cogen[BandNormalized[T]] =
    Cogen[(UnnormalizedSED, Map[Band, BrightnessMeasureOverTime[T]])].contramap(x =>
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
    Cogen[(Map[Wavelength, EmissionLine[T]], Measure[PosBigDecimal])].contramap(x =>
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
