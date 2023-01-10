// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.enums._
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbStepConfig {
  import ArbEnumerated._
  import ArbOffset._

  implicit val arbStepConfigGcal: Arbitrary[StepConfig.Gcal] = Arbitrary(
    for {
      continuum <- arbitrary[Option[GcalContinuum]]
      arcs      <- arbitrary[List[GcalArc]]
      filter    <- arbitrary[GcalFilter]
      diffuser  <- arbitrary[GcalDiffuser]
      shutter   <- arbitrary[GcalShutter]
    } yield StepConfig.Gcal(continuum, arcs, filter, diffuser, shutter)
  )

  implicit val cogStepConfigGcal: Cogen[StepConfig.Gcal] =
    Cogen[(Option[GcalContinuum], List[GcalArc], GcalFilter, GcalDiffuser, GcalShutter)].contramap(
      c => (c.continuum, c.arcs, c.filter, c.diffuser, c.shutter)
    )

  implicit val arbStepConfigScience: Arbitrary[StepConfig.Science] =
    Arbitrary(arbitrary[Offset].map(StepConfig.Science.apply))

  implicit val cogStepConfigScience: Cogen[StepConfig.Science] = Cogen[Offset].contramap(_.offset)

  implicit val arbStepConfig: Arbitrary[StepConfig] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(StepConfig.Bias),
        Gen.const(StepConfig.Dark),
        arbitrary[StepConfig.Gcal],
        arbitrary[StepConfig.Science]
      )
    )

  implicit val cogStepConfig: Cogen[StepConfig] =
    Cogen[Either[Unit, Either[Unit, Either[StepConfig.Gcal, StepConfig.Science]]]].contramap {
      case StepConfig.Bias                    => ().asLeft
      case StepConfig.Dark                    => ().asLeft.asRight
      case g @ StepConfig.Gcal(_, _, _, _, _) => g.asLeft.asRight.asRight
      case s @ StepConfig.Science(_)          => s.asRight.asRight.asRight
    }
}

object ArbStepConfig extends ArbStepConfig
