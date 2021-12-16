// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import coulomb._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.math.units._
import lucuma.core.model.EmissionLine
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbEmissionLine {
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbQty._
  import ArbWavelength._
  import ArbRefined._

  implicit def arbEmissionLine[T](implicit
    arbLineFluxUnit: Arbitrary[GroupedUnitType[LineFlux[T]]]
  ): Arbitrary[EmissionLine[T]] =
    Arbitrary {
      for {
        w  <- arbitrary[Wavelength]
        lw <- arbitrary[PosBigDecimal]
        lf <- arbitrary[GroupedUnitQty[PosBigDecimal, LineFlux[T]]]
      } yield EmissionLine[T](w, lw.withUnit[KilometersPerSecond], lf)
    }

  implicit def cogEmissionLine[T](implicit
    cogenLineFluxUnit: Cogen[GroupedUnitType[LineFlux[T]]]
  ): Cogen[EmissionLine[T]] =
    Cogen[
      (Wavelength, PosBigDecimal, GroupedUnitQty[PosBigDecimal, LineFlux[T]])
    ].contramap(x => (x.wavelength, x.lineWidth.value, x.lineFlux))
}

object ArbEmissionLine extends ArbEmissionLine
