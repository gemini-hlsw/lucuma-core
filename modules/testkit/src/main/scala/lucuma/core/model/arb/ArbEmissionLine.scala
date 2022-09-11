// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.Order._
import cats.laws.discipline.arbitrary.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units._
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbEmissionLine {
  import ArbEnumerated._
  import ArbTimestamp._
  import BrightnessUnits._
  import ArbMeasure._
  import ArbRefined._
  import ArbSpectralDefinition._

  implicit def arbEmissionLine[T](implicit
    arbLineFluxUnit: Arbitrary[Units Of LineFlux[T]]
  ): Arbitrary[EmissionLine[T]] =
    Arbitrary(
      for {
        lw <- arbitrary[PosBigDecimal]
        lf <- arbitrary[EmissionLine.LineFluxOverTime[T]]
      } yield EmissionLine[T](lw.withUnit[KilometersPerSecond], lf)
    )

  implicit def cogEmissionLine[T]: Cogen[EmissionLine[T]] =
    Cogen[(PosBigDecimal, EmissionLine.LineFluxOverTime[T])]
      .contramap(x => (x.lineWidth.value, x.lineFlux))
}

object ArbEmissionLine extends ArbEmissionLine
