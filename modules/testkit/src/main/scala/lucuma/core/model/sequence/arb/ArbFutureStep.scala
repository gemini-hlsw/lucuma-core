// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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

trait ArbFutureStep {
  import ArbUid._
  import ArbDynamicConfig._
  import ArbStepConfig._
  import ArbStepTime._
  import ArbEnumerated._

  implicit val arbFutureStepGmosNorth: Arbitrary[FutureStep.GmosNorth] = Arbitrary(
    for {
      id               <- arbitrary[Step.Id]
      instrumentConfig <- arbitrary[DynamicConfig.GmosNorth]
      stepConfig       <- arbitrary[StepConfig]
      time             <- arbitrary[StepTime]
      breakpoint       <- arbitrary[Breakpoint]
    } yield FutureStep.GmosNorth(id, instrumentConfig, stepConfig, time, breakpoint)
  )

  implicit val cogFutureStepGmosNorth: Cogen[FutureStep.GmosNorth] =
    Cogen[(Step.Id, DynamicConfig.GmosNorth, StepConfig, StepTime, Breakpoint)].contramap(s =>
      (s.id, s.instrumentConfig, s.stepConfig, s.time, s.breakpoint)
    )

  implicit val arbFutureStepGmosSouth: Arbitrary[FutureStep.GmosSouth] = Arbitrary(
    for {
      id               <- arbitrary[Step.Id]
      instrumentConfig <- arbitrary[DynamicConfig.GmosSouth]
      stepConfig       <- arbitrary[StepConfig]
      time             <- arbitrary[StepTime]
      breakpoint       <- arbitrary[Breakpoint]
    } yield FutureStep.GmosSouth(id, instrumentConfig, stepConfig, time, breakpoint)
  )

  implicit val cogFutureStepGmosSouth: Cogen[FutureStep.GmosSouth] =
    Cogen[(Step.Id, DynamicConfig.GmosSouth, StepConfig, StepTime, Breakpoint)].contramap(s =>
      (s.id, s.instrumentConfig, s.stepConfig, s.time, s.breakpoint)
    )

  implicit val arbFutureStep: Arbitrary[FutureStep] = Arbitrary(
    Gen.oneOf(
      arbitrary[FutureStep.GmosNorth],
      arbitrary[FutureStep.GmosSouth]
    )
  )

  implicit val cogFutureStep: Cogen[FutureStep] =
    Cogen[Either[FutureStep.GmosNorth, FutureStep.GmosSouth]].contramap {
      case s: FutureStep.GmosNorth => s.asLeft
      case s: FutureStep.GmosSouth => s.asRight
    }
}

object ArbFutureStep extends ArbFutureStep
