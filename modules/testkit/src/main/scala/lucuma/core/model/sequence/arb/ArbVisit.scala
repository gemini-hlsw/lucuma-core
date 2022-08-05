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

trait ArbVisit {
  import ArbUid._
  import ArbEnumerated._
  import ArbStaticConfig._
  import ArbStepRecord._
  import ArbTime._

  implicit val arbVisitGmosNorth: Arbitrary[Visit.GmosNorth] = Arbitrary(
    for {
      id           <- arbitrary[Visit.Id]
      created      <- arbitrary[Instant]
      staticConfig <- arbitrary[StaticConfig.GmosNorth]
      steps        <- arbitrary[List[StepRecord.GmosNorth]]
    } yield Visit.GmosNorth(id, created, staticConfig, steps)
  )

  implicit val cogVisitGmosNorth: Cogen[Visit.GmosNorth] =
    Cogen[(Visit.Id, Instant, StaticConfig.GmosNorth, List[StepRecord.GmosNorth])].contramap(s =>
      (s.id, s.created, s.staticConfig, s.steps)
    )

  implicit val arbVisitGmosSouth: Arbitrary[Visit.GmosSouth] = Arbitrary(
    for {
      id           <- arbitrary[Visit.Id]
      created      <- arbitrary[Instant]
      staticConfig <- arbitrary[StaticConfig.GmosSouth]
      steps        <- arbitrary[List[StepRecord.GmosSouth]]
    } yield Visit.GmosSouth(id, created, staticConfig, steps)
  )

  implicit val cogVisitGmosSouth: Cogen[Visit.GmosSouth] =
    Cogen[(Visit.Id, Instant, StaticConfig.GmosSouth, List[StepRecord.GmosSouth])].contramap(s =>
      (s.id, s.created, s.staticConfig, s.steps)
    )

  implicit val arbVisit: Arbitrary[Visit] = Arbitrary(
    Gen.oneOf(
      arbitrary[Visit.GmosNorth],
      arbitrary[Visit.GmosSouth]
    )
  )

  implicit val cogVisit: Cogen[Visit] =
    Cogen[Either[Visit.GmosNorth, Visit.GmosSouth]].contramap {
      case s: Visit.GmosNorth => s.asLeft
      case s: Visit.GmosSouth => s.asRight
    }
}

object ArbVisit extends ArbVisit
