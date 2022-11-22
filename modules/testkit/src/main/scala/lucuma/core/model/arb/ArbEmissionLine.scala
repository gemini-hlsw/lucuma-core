// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.Order.*
import cats.laws.discipline.arbitrary.*
import org.typelevel.cats.time.instantInstances
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.*
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbEmissionLine {
  import ArbEnumerated.*
  import ArbTimestamp.*
  import BrightnessUnits.*
  import ArbMeasure.*
  import ArbRefined.*
  import ArbSpectralDefinition.*

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
