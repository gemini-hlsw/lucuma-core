// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package arb

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbStaticConfig {
  import ArbEnumerated._
  import ArbGmosNodAndShuffle._

  implicit val arbStaticConfigGmosNorth: Arbitrary[StaticConfig.GmosNorth] = Arbitrary(
    for {
      stageMode     <- arbitrary[GmosNorthStageMode]
      detector      <- arbitrary[GmosNorthDetector]
      mosPreImaging <- arbitrary[MosPreImaging]
      nodAndShuffle <- arbitrary[Option[GmosNodAndShuffle]]
    } yield StaticConfig.GmosNorth(stageMode, detector, mosPreImaging, nodAndShuffle)
  )

  implicit val cogStaticConfigGmosNorth: Cogen[StaticConfig.GmosNorth] =
    Cogen[(GmosNorthStageMode, GmosNorthDetector, MosPreImaging, Option[GmosNodAndShuffle])]
      .contramap(s => (s.stageMode, s.detector, s.mosPreImaging, s.nodAndShuffle))

  implicit val arbStaticConfigGmosSouth: Arbitrary[StaticConfig.GmosSouth] = Arbitrary(
    for {
      stageMode     <- arbitrary[GmosSouthStageMode]
      detector      <- arbitrary[GmosSouthDetector]
      mosPreImaging <- arbitrary[MosPreImaging]
      nodAndShuffle <- arbitrary[Option[GmosNodAndShuffle]]
    } yield StaticConfig.GmosSouth(stageMode, detector, mosPreImaging, nodAndShuffle)
  )

  implicit val cogStaticConfigGmosSouth: Cogen[StaticConfig.GmosSouth] =
    Cogen[(GmosSouthStageMode, GmosSouthDetector, MosPreImaging, Option[GmosNodAndShuffle])]
      .contramap(s => (s.stageMode, s.detector, s.mosPreImaging, s.nodAndShuffle))

  implicit val arbStaticConfig: Arbitrary[StaticConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[StaticConfig.GmosNorth],
      arbitrary[StaticConfig.GmosSouth]
    )
  )

  implicit val cogStaticConfig: Cogen[StaticConfig] =
    Cogen[Either[StaticConfig.GmosNorth, StaticConfig.GmosSouth]]
      .contramap {
        case s @ StaticConfig.GmosNorth(_, _, _, _) => s.asLeft
        case s @ StaticConfig.GmosSouth(_, _, _, _) => s.asRight
      }
}

object ArbStaticConfig extends ArbStaticConfig
