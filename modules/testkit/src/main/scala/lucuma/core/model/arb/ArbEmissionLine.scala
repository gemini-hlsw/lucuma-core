// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units._
import lucuma.core.model.EmissionLine
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbEmissionLine {
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbMeasure._
  import ArbRefined._

  implicit def arbEmissionLine[T](implicit
    arbLineFluxUnit: Arbitrary[Units Of LineFlux[T]]
  ): Arbitrary[EmissionLine[T]] =
    Arbitrary(
      for {
        lw <- arbitrary[PosBigDecimal]
        lf <- arbitrary[Measure[PosBigDecimal] Of LineFlux[T]]
      } yield EmissionLine[T](lw.withUnit[KilometersPerSecond], lf)
    )

  implicit def cogEmissionLine[T]: Cogen[EmissionLine[T]] =
    Cogen[(PosBigDecimal, Measure[PosBigDecimal])].contramap(x => (x.lineWidth.value, x.lineFlux))
}

object ArbEmissionLine extends ArbEmissionLine
