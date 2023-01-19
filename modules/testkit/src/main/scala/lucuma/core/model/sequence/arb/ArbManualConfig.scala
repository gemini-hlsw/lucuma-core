// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.arb.ArbTime
import lucuma.core.model.sequence._
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbUid
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbManualConfig {
  import ArbAtom._
  import ArbTimeSpan.given
  import ArbStaticConfig._
  import ArbTime._
  import ArbUid._

  implicit val arbManualConfigGmosNorth: Arbitrary[ManualConfig.GmosNorth] = Arbitrary(
    for {
      static      <- arbitrary[StaticConfig.GmosNorth]
      setupTime   <- arbitrary[TimeSpan]
      acquisition <- arbitrary[List[Atom.GmosNorth]]
      science     <- arbitrary[List[Atom.GmosNorth]]
    } yield ManualConfig.GmosNorth(static, setupTime, acquisition, science)
  )

  implicit val cogManualConfigGmosNorth: Cogen[ManualConfig.GmosNorth] =
    Cogen[(StaticConfig.GmosNorth, TimeSpan, List[Atom.GmosNorth], List[Atom.GmosNorth])].contramap(
      c => (c.static, c.setupTime, c.acquisition, c.science)
    )

  implicit val arbManualConfigGmosSouth: Arbitrary[ManualConfig.GmosSouth] = Arbitrary(
    for {
      static      <- arbitrary[StaticConfig.GmosSouth]
      setupTime   <- arbitrary[TimeSpan]
      acquisition <- arbitrary[List[Atom.GmosSouth]]
      science     <- arbitrary[List[Atom.GmosSouth]]
    } yield ManualConfig.GmosSouth(static, setupTime, acquisition, science)
  )

  implicit val cogManualConfigGmosSouth: Cogen[ManualConfig.GmosSouth] =
    Cogen[(StaticConfig.GmosSouth, TimeSpan, List[Atom.GmosSouth], List[Atom.GmosSouth])].contramap(
      c => (c.static, c.setupTime, c.acquisition, c.science)
    )

  implicit val arbManualConfig: Arbitrary[ManualConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[ManualConfig.GmosNorth],
      arbitrary[ManualConfig.GmosSouth]
    )
  )

  implicit val cogManualConfig: Cogen[ManualConfig] =
    Cogen[Either[ManualConfig.GmosNorth, ManualConfig.GmosSouth]].contramap {
      case s @ ManualConfig.GmosNorth(_, _, _, _) => s.asLeft
      case s @ ManualConfig.GmosSouth(_, _, _, _) => s.asRight
    }
}

object ArbManualConfig extends ArbManualConfig
