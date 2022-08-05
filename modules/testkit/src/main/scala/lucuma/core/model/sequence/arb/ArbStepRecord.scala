// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Instant

trait ArbStepRecord {
  import ArbUid._
  import ArbDynamicConfig._
  import ArbStepConfig._
  import ArbTime._
  import ArbEnumerated._

  implicit val arbStepRecordGmosNorth: Arbitrary[StepRecord.GmosNorth] = Arbitrary(
    for {
      id               <- arbitrary[Step.Id]
      created          <- arbitrary[Instant]
      startTime        <- arbitrary[Option[Instant]]
      endTime          <- arbitrary[Option[Instant]]
      instrumentConfig <- arbitrary[DynamicConfig.GmosNorth]
      stepConfig       <- arbitrary[StepConfig]
    } yield StepRecord.GmosNorth(id, created, startTime, endTime, instrumentConfig, stepConfig)
  )

  implicit val cogStepRecordGmosNorth: Cogen[StepRecord.GmosNorth] =
    Cogen[(Step.Id, Instant, Option[Instant], Option[Instant], DynamicConfig.GmosNorth, StepConfig)]
      .contramap(s => (s.id, s.created, s.startTime, s.endTime, s.instrumentConfig, s.stepConfig))

  implicit val arbStepRecordGmosSouth: Arbitrary[StepRecord.GmosSouth] = Arbitrary(
    for {
      id               <- arbitrary[Step.Id]
      created          <- arbitrary[Instant]
      startTime        <- arbitrary[Option[Instant]]
      endTime          <- arbitrary[Option[Instant]]
      instrumentConfig <- arbitrary[DynamicConfig.GmosSouth]
      stepConfig       <- arbitrary[StepConfig]
    } yield StepRecord.GmosSouth(id, created, startTime, endTime, instrumentConfig, stepConfig)
  )

  implicit val cogStepRecordGmosSouth: Cogen[StepRecord.GmosSouth] =
    Cogen[(Step.Id, Instant, Option[Instant], Option[Instant], DynamicConfig.GmosSouth, StepConfig)]
      .contramap(s => (s.id, s.created, s.startTime, s.endTime, s.instrumentConfig, s.stepConfig))

  implicit val arbStepRecord: Arbitrary[StepRecord] = Arbitrary(
    Gen.oneOf(
      arbitrary[StepRecord.GmosNorth],
      arbitrary[StepRecord.GmosSouth]
    )
  )

  implicit val cogStepRecord: Cogen[StepRecord] =
    Cogen[Either[StepRecord.GmosNorth, StepRecord.GmosSouth]].contramap {
      case s: StepRecord.GmosNorth => s.asLeft
      case s: StepRecord.GmosSouth => s.asRight
    }
}

object ArbStepRecord extends ArbStepRecord
