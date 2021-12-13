// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.implicits._
import coulomb.refined._
import coulomb.si.Kelvin
import eu.timepit.refined.numeric.Positive
import lucuma.core.enum.NonStellarLibrarySpectrum
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnit
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbSpectralDistribution {
  import ArbEnumerated._
  import SpectralDistribution._
  import BrightnessUnit._
  import ArbQty._

  implicit val arbLibrary: Arbitrary[Library] =
    Arbitrary {
      for {
        s <- arbitrary[StellarLibrarySpectrum].map(_.asLeft)
        l <- arbitrary[NonStellarLibrarySpectrum].map(_.asRight)
        a <- Gen.oneOf(s, l)
      } yield Library(a)
    }

  implicit val cogLibrary: Cogen[Library] =
    Cogen[Either[StellarLibrarySpectrum, NonStellarLibrarySpectrum]].contramap(_.librarySpectrum)

  implicit val arbCoolStarModel: Arbitrary[CoolStarModel] =
    Arbitrary {
      for {
        a <- Gen.choose(BigDecimal(1), BigDecimal(10000))
      } yield CoolStarModel(a.withRefinedUnit[Positive, Kelvin])
    }

  implicit val cogCoolStarModel: Cogen[CoolStarModel] =
    Cogen[BigDecimal].contramap(_.temperature.value.value)

  implicit def arbEmissionLine[T](implicit
    arbLineUnit:      Arbitrary[GroupedUnitType[LineFlux[T]]],
    arbContinuumUnit: Arbitrary[GroupedUnitType[ContinuumFluxDensity[T]]]
  ): Arbitrary[EmissionLine[T]] =
    Arbitrary {
      for {
        l <- arbitrary[GroupedUnitQuantity[BigDecimal, LineFlux[T]]]
        c <- arbitrary[GroupedUnitQuantity[BigDecimal, ContinuumFluxDensity[T]]]
      } yield EmissionLine[T](l, c)
    }

  implicit def cogEmissionLine[T](implicit
    cogenLineUnit:      Cogen[GroupedUnitType[LineFlux[T]]],
    cogenContinuumUnit: Cogen[GroupedUnitType[ContinuumFluxDensity[T]]]
  ): Cogen[EmissionLine[T]] =
    Cogen[
      (
        GroupedUnitQuantity[BigDecimal, LineFlux[T]],
        GroupedUnitQuantity[BigDecimal, ContinuumFluxDensity[T]]
      )
    ].contramap(x => (x.line, x.continuum))

  implicit val arbPowerLaw: Arbitrary[PowerLaw] =
    Arbitrary {
      for {
        a <- arbitrary[BigDecimal]
      } yield PowerLaw(a)
    }

  implicit val cogPowerLaw: Cogen[PowerLaw] =
    Cogen[BigDecimal].contramap(_.index)

  implicit val arbBlackBody: Arbitrary[BlackBody] =
    Arbitrary {
      for {
        a <- Gen.choose(BigDecimal(1), BigDecimal(10000))
      } yield BlackBody(a.withRefinedUnit[Positive, Kelvin])
    }

  implicit val cogBlackBody: Cogen[BlackBody] =
    Cogen[BigDecimal].contramap(_.temperature.value.value)

  implicit def arbSpectralDistribution[T](implicit
    arbLineUnit:      Arbitrary[GroupedUnitType[LineFlux[T]]],
    arbContinuumUnit: Arbitrary[GroupedUnitType[ContinuumFluxDensity[T]]]
  ): Arbitrary[SpectralDistribution[T]] =
    Arbitrary {
      for {
        r <- Gen.oneOf(
               arbitrary[Library],
               arbitrary[CoolStarModel],
               arbitrary[EmissionLine[T]],
               arbitrary[PowerLaw],
               arbitrary[BlackBody]
             )
      } yield r
    }

  implicit def cogSpectralDistribution[T](implicit
    cogenLineUnit:      Cogen[GroupedUnitType[LineFlux[T]]],
    cogenContinuumUnit: Cogen[GroupedUnitType[ContinuumFluxDensity[T]]]
  ): Cogen[SpectralDistribution[T]] =
    Cogen[Either[
      Library,
      Either[
        CoolStarModel,
        Either[
          EmissionLine[T],
          Either[
            PowerLaw,
            BlackBody,
          ]
        ]
      ]
    ]].contramap {
      case d @ Library(_)         => d.asLeft
      case d @ CoolStarModel(_)   => d.asLeft.asRight
      case d @ EmissionLine(_, _) => d.asLeft.asRight.asRight
      case d @ PowerLaw(_)        => d.asLeft.asRight.asRight.asRight
      case d @ BlackBody(_)       => d.asRight.asRight.asRight.asRight
    }
}

object ArbSpectralDistribution extends ArbSpectralDistribution
