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
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbUnnormalizedSpectralEnergyDistribution {
  import ArbEnumerated._
  import USED._
  import ArbRefined._
  import ArbWavelength._

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

  implicit def arbSpectralDistribution[T]: Arbitrary[USED] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[StellarLibrary],
        arbitrary[CoolStarModel],
        arbitrary[Galaxy],
        arbitrary[Planet],
        arbitrary[Quasar],
        arbitrary[HIIRegion],
        arbitrary[PlanetaryNebula],
        arbitrary[PowerLaw],
        arbitrary[BlackBody],
        arbitrary[UserDefined]
      )
    )

  implicit def cogSpectralDistribution[T]: Cogen[USED] =
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
    ]].contramap {
      case d @ StellarLibrary(_)  => d.asLeft
      case d @ CoolStarModel(_)   => d.asLeft.asRight
      case d @ Galaxy(_)          => d.asLeft.asRight.asRight
      case d @ Planet(_)          => d.asLeft.asRight.asRight.asRight
      case d @ Quasar(_)          => d.asLeft.asRight.asRight.asRight.asRight
      case d @ HIIRegion(_)       => d.asLeft.asRight.asRight.asRight.asRight.asRight
      case d @ PlanetaryNebula(_) => d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ PowerLaw(_)        => d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ BlackBody(_)       =>
        d.asLeft.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
      case d @ UserDefined(_)     =>
        d.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight.asRight
    }
}

object ArbUnnormalizedSpectralEnergyDistribution extends ArbUnnormalizedSpectralEnergyDistribution
