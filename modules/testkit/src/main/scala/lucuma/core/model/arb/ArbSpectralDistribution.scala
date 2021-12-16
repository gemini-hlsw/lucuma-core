// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.data.NonEmptyMap
import cats.implicits._
import cats.laws.discipline.arbitrary._
import coulomb.refined._
import coulomb.si.Kelvin
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enum._
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbSpectralDistribution {
  import ArbEnumerated._
  import SpectralDistribution._
  import BrightnessUnits._
  import ArbQty._
  import ArbEmissionLine._
  import ArbRefined._
  import ArbWavelength._

  implicit def arbEmissionLineMap[T](implicit
    arbLineFluxUnit: Arbitrary[GroupedUnitType[LineFlux[T]]]
  ): Arbitrary[SortedMap[Wavelength, EmissionLine[T]]] =
    Arbitrary(
      arbitrary[Vector[EmissionLine[T]]]
        .map(_.fproductLeft(_.wavelength))
        .map(x => SortedMap(x: _*))
    )

  implicit def cogEmissionLineMap[T](implicit
    cogenLineUnit: Cogen[GroupedUnitType[LineFlux[T]]]
  ): Cogen[SortedMap[Wavelength, EmissionLine[T]]] =
    Cogen[Vector[(Wavelength, EmissionLine[T])]].contramap(_.toVector)

  implicit val arbStellarLibrary: Arbitrary[StellarLibrary] =
    Arbitrary(arbitrary[StellarLibrarySpectrum].map(StellarLibrary(_)))

  implicit val cogStellarLibrary: Cogen[StellarLibrary] =
    Cogen[StellarLibrarySpectrum].contramap(_.librarySpectrum)

  implicit val arbCoolStarModel: Arbitrary[CoolStarModel] =
    Arbitrary(
      Gen
        .choose(BigDecimal(1), BigDecimal(10000))
        .map(a => CoolStarModel(a.withRefinedUnit[Positive, Kelvin]))
    )

  implicit val cogCoolStarModel: Cogen[CoolStarModel] =
    Cogen[BigDecimal].contramap(_.temperature.value.value)

  implicit val arbGalaxy: Arbitrary[Galaxy] =
    Arbitrary(arbitrary[GalaxySpectrum].map(Galaxy(_)))

  implicit val cogGalaxy: Cogen[Galaxy] =
    Cogen[GalaxySpectrum].contramap(_.galaxySpectrum)

  implicit val arbPlanet: Arbitrary[Planet] =
    Arbitrary(arbitrary[PlanetSpectrum].map(Planet(_)))

  implicit val cogPlanet: Cogen[Planet] =
    Cogen[PlanetSpectrum].contramap(_.planetSpectrum)

  implicit val arbQuasar: Arbitrary[Quasar] =
    Arbitrary(arbitrary[QuasarSpectrum].map(Quasar(_)))

  implicit val cogQuasar: Cogen[Quasar] =
    Cogen[QuasarSpectrum].contramap(_.quasarSpectrum)

  implicit val arbHIIRegion: Arbitrary[HIIRegion] =
    Arbitrary(arbitrary[HIIRegionSpectrum].map(HIIRegion(_)))

  implicit val cogHIIRegion: Cogen[HIIRegion] =
    Cogen[HIIRegionSpectrum].contramap(_.hiiRegionSpectrum)

  implicit val arbPlanetaryNebula: Arbitrary[PlanetaryNebula] =
    Arbitrary(arbitrary[PlanetaryNebulaSpectrum].map(PlanetaryNebula(_)))

  implicit val cogPlanetaryNebula: Cogen[PlanetaryNebula] =
    Cogen[PlanetaryNebulaSpectrum].contramap(_.planetaryNebulaSpectrum)

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

  implicit val arbPowerLaw: Arbitrary[PowerLaw] =
    Arbitrary(arbitrary[BigDecimal].map(PowerLaw(_)))

  implicit val cogPowerLaw: Cogen[PowerLaw] =
    Cogen[BigDecimal].contramap(_.index)

  implicit val arbBlackBody: Arbitrary[BlackBody] =
    Arbitrary(
      Gen
        .choose(BigDecimal(1), BigDecimal(10000))
        .map(a => BlackBody(a.withRefinedUnit[Positive, Kelvin]))
    )

  implicit val cogBlackBody: Cogen[BlackBody] =
    Cogen[BigDecimal].contramap(_.temperature.value.value)

  implicit val arbUserDefined: Arbitrary[UserDefined] =
    Arbitrary(
      arbitrary[NonEmptyMap[Wavelength, PosBigDecimal]].map(UserDefined(_))
    )

  implicit val cogUserDefined: Cogen[UserDefined] =
    Cogen[Map[Wavelength, PosBigDecimal]].contramap(_.fluxDensities.toSortedMap)

  implicit def arbSpectralDistribution[T](implicit
    arbLineUnit:      Arbitrary[GroupedUnitType[LineFlux[T]]],
    arbContinuumUnit: Arbitrary[GroupedUnitType[FluxDensityContinuum[T]]]
  ): Arbitrary[SpectralDistribution[T]] =
    Arbitrary {
      for {
        r <- Gen.oneOf(
               arbitrary[StellarLibrary],
               arbitrary[CoolStarModel],
               arbitrary[Galaxy],
               arbitrary[Planet],
               arbitrary[Quasar],
               arbitrary[HIIRegion],
               arbitrary[PlanetaryNebula],
               arbitrary[EmissionLines[T]],
               arbitrary[PowerLaw],
               arbitrary[BlackBody],
               arbitrary[UserDefined]
             )
      } yield r
    }

  implicit def cogSpectralDistribution[T](implicit
    cogenLineUnit:      Cogen[GroupedUnitType[LineFlux[T]]],
    cogenContinuumUnit: Cogen[GroupedUnitType[FluxDensityContinuum[T]]]
  ): Cogen[SpectralDistribution[T]] =
    Cogen[Either[
      StellarLibrary,
      Either[
        CoolStarModel,
        Either[
          Galaxy,
          Either[
            Planet,
            Either[
              Quasar,
              Either[
                HIIRegion,
                Either[
                  PlanetaryNebula,
                  Either[
                    EmissionLines[T],
                    Either[
                      PowerLaw,
                      Either[
                        BlackBody,
                        UserDefined
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]].contramap {
      case d @ StellarLibrary(_)   => d.asLeft
      case d @ CoolStarModel(_)    => d.asLeft.asRight
      case d @ Galaxy(_)           => d.asLeft.asRight.asRight
      case d @ Planet(_)           => d.asLeft.asRight.asRight.asRight
      case d @ Quasar(_)           => d.asLeft.asRight.asRight.asRight.asRight
      case d @ HIIRegion(_)        => d.asLeft.asRight.asRight.asRight.asRight.asRight
      case d @ PlanetaryNebula(_)  => d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ EmissionLines(_, _) =>
        d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ PowerLaw(_)         =>
        d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ BlackBody(_)        =>
        d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ UserDefined(_)      =>
        d.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
    }
}

object ArbSpectralDistribution extends ArbSpectralDistribution
