// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.model.sequence._
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbFutureExecutionConfig {
  import ArbUid._
  import ArbStaticConfig._
  import ArbExecutionSequence._

  implicit val arbFutureExecutionConfigGmosNorth: Arbitrary[FutureExecutionConfig.GmosNorth] =
    Arbitrary(
      for {
        static      <- arbitrary[StaticConfig.GmosNorth]
        acquisition <- arbitrary[ExecutionSequence.GmosNorth]
        science     <- arbitrary[ExecutionSequence.GmosNorth]
      } yield FutureExecutionConfig.GmosNorth(static, acquisition, science)
    )

  implicit val cogFutureExecutionConfigGmosNorth: Cogen[FutureExecutionConfig.GmosNorth] =
    Cogen[(StaticConfig.GmosNorth, ExecutionSequence.GmosNorth, ExecutionSequence.GmosNorth)]
      .contramap(c => (c.static, c.acquisition, c.science))

  implicit val arbFutureExecutionConfigGmosSouth: Arbitrary[FutureExecutionConfig.GmosSouth] =
    Arbitrary(
      for {
        static      <- arbitrary[StaticConfig.GmosSouth]
        acquisition <- arbitrary[ExecutionSequence.GmosSouth]
        science     <- arbitrary[ExecutionSequence.GmosSouth]
      } yield FutureExecutionConfig.GmosSouth(static, acquisition, science)
    )

  implicit val cogFutureExecutionConfigGmosSouth: Cogen[FutureExecutionConfig.GmosSouth] =
    Cogen[(StaticConfig.GmosSouth, ExecutionSequence.GmosSouth, ExecutionSequence.GmosSouth)]
      .contramap(c => (c.static, c.acquisition, c.science))

  implicit val arbFutureExecutionConfig: Arbitrary[FutureExecutionConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[FutureExecutionConfig.GmosNorth],
      arbitrary[FutureExecutionConfig.GmosSouth]
    )
  )

  implicit val cogFutureExecutionConfig: Cogen[FutureExecutionConfig] =
    Cogen[Either[FutureExecutionConfig.GmosNorth, FutureExecutionConfig.GmosSouth]].contramap {
      case s @ FutureExecutionConfig.GmosNorth(_, _, _) => s.asLeft
      case s @ FutureExecutionConfig.GmosSouth(_, _, _) => s.asRight
    }
}

object ArbFutureExecutionConfig extends ArbFutureExecutionConfig
