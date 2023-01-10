// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.enums.Breakpoint
import lucuma.core.model.sequence._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbStep {
  import ArbUid._
  import ArbDynamicConfig._
  import ArbStepConfig._
  import ArbStepTime._
  import ArbEnumerated._

  implicit val arbStepGmosNorth: Arbitrary[Step.GmosNorth] = Arbitrary(
    for {
      id            <- arbitrary[Step.Id]
      DynamicConfig <- arbitrary[DynamicConfig.GmosNorth]
      stepConfig    <- arbitrary[StepConfig]
      time          <- arbitrary[StepTime]
      breakpoint    <- arbitrary[Breakpoint]
    } yield Step.GmosNorth(id, DynamicConfig, stepConfig, time, breakpoint)
  )

  implicit val cogStepGmosNorth: Cogen[Step.GmosNorth] =
    Cogen[(Step.Id, DynamicConfig.GmosNorth, StepConfig, StepTime, Breakpoint)].contramap(s =>
      (s.id, s.instrumentConfig, s.stepConfig, s.time, s.breakpoint)
    )

  implicit val arbStepGmosSouth: Arbitrary[Step.GmosSouth] = Arbitrary(
    for {
      id            <- arbitrary[Step.Id]
      DynamicConfig <- arbitrary[DynamicConfig.GmosSouth]
      stepConfig    <- arbitrary[StepConfig]
      time          <- arbitrary[StepTime]
      breakpoint    <- arbitrary[Breakpoint]
    } yield Step.GmosSouth(id, DynamicConfig, stepConfig, time, breakpoint)
  )

  implicit val cogStepGmosSouth: Cogen[Step.GmosSouth] =
    Cogen[(Step.Id, DynamicConfig.GmosSouth, StepConfig, StepTime, Breakpoint)].contramap(s =>
      (s.id, s.instrumentConfig, s.stepConfig, s.time, s.breakpoint)
    )

  implicit val arbStep: Arbitrary[Step] = Arbitrary(
    Gen.oneOf(
      arbitrary[Step.GmosNorth],
      arbitrary[Step.GmosSouth]
    )
  )

  implicit val cogStep: Cogen[Step] = Cogen[Either[Step.GmosNorth, Step.GmosSouth]].contramap {
    case s: Step.GmosNorth => s.asLeft
    case s: Step.GmosSouth => s.asRight
  }
}

object ArbStep extends ArbStep
