// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.data.NonEmptyMap
import cats.implicits.*
import cats.laws.discipline.arbitrary.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Kelvin
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.units.*
import lucuma.core.util.arb.ArbEnumerated
import lucuma.refined.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbUnnormalizedSED {
  import ArbEnumerated.*
  import UnnormalizedSED.*
  import ArbRefined.*
  import ArbWavelength.*

  given Arbitrary[StellarLibrary] =
    Arbitrary(arbitrary[StellarLibrarySpectrum].map(StellarLibrary(_)))

  given Cogen[StellarLibrary] =
    Cogen[StellarLibrarySpectrum].contramap(_.librarySpectrum)

  given Arbitrary[CoolStarModel] =
    Arbitrary(arbitrary[CoolStarTemperature].map(CoolStarModel(_)))

  given Cogen[CoolStarModel] =
    Cogen[BigDecimal].contramap(_.temperature.temperature.value.value)

  given Arbitrary[Galaxy] =
    Arbitrary(arbitrary[GalaxySpectrum].map(Galaxy(_)))

  given Cogen[Galaxy] =
    Cogen[GalaxySpectrum].contramap(_.galaxySpectrum)

  given Arbitrary[Planet] =
    Arbitrary(arbitrary[PlanetSpectrum].map(Planet(_)))

  given Cogen[Planet] =
    Cogen[PlanetSpectrum].contramap(_.planetSpectrum)

  given Arbitrary[Quasar] =
    Arbitrary(arbitrary[QuasarSpectrum].map(Quasar(_)))

  given Cogen[Quasar] =
    Cogen[QuasarSpectrum].contramap(_.quasarSpectrum)

  given Arbitrary[HIIRegion] =
    Arbitrary(arbitrary[HIIRegionSpectrum].map(HIIRegion(_)))

  given Cogen[HIIRegion] =
    Cogen[HIIRegionSpectrum].contramap(_.hiiRegionSpectrum)

  given Arbitrary[PlanetaryNebula] =
    Arbitrary(arbitrary[PlanetaryNebulaSpectrum].map(PlanetaryNebula(_)))

  given Cogen[PlanetaryNebula] =
    Cogen[PlanetaryNebulaSpectrum].contramap(_.planetaryNebulaSpectrum)

  given Arbitrary[PowerLaw] =
    Arbitrary(arbitrary[BigDecimal].map(PowerLaw(_)))

  given Cogen[PowerLaw] =
    Cogen[BigDecimal].contramap(_.index)

  given Arbitrary[BlackBody] =
    Arbitrary(
      Gen
        .choose(1, 10000)
        .map(a => BlackBody(refineV[Positive](a).toOption.get.withUnit[Kelvin]))
    )

  given Cogen[BlackBody] =
    Cogen[BigDecimal].contramap(_.temperature.value.value)

  given Arbitrary[UserDefined] =
    Arbitrary(
      arbitrary[NonEmptyMap[Wavelength, PosBigDecimal]].map(UserDefined(_))
    )

  given Cogen[UserDefined] =
    Cogen[Map[Wavelength, PosBigDecimal]].contramap(_.fluxDensities.toSortedMap)

  given Arbitrary[UnnormalizedSED] =
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

  given Cogen[UnnormalizedSED] =
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

object ArbUnnormalizedSED extends ArbUnnormalizedSED
