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
  import ArbEnumerated.given
  import ArbGmosNodAndShuffle.given

  given Arbitrary[StaticConfig.GmosNorth] = Arbitrary(
    for {
      stageMode     <- arbitrary[GmosNorthStageMode]
      detector      <- arbitrary[GmosNorthDetector]
      mosPreImaging <- arbitrary[MosPreImaging]
      nodAndShuffle <- arbitrary[Option[GmosNodAndShuffle]]
    } yield StaticConfig.GmosNorth(stageMode, detector, mosPreImaging, nodAndShuffle)
  )

  given Cogen[StaticConfig.GmosNorth] =
    Cogen[(GmosNorthStageMode, GmosNorthDetector, MosPreImaging, Option[GmosNodAndShuffle])]
      .contramap(s => (s.stageMode, s.detector, s.mosPreImaging, s.nodAndShuffle))

  given Arbitrary[StaticConfig.GmosSouth] = Arbitrary(
    for {
      stageMode     <- arbitrary[GmosSouthStageMode]
      detector      <- arbitrary[GmosSouthDetector]
      mosPreImaging <- arbitrary[MosPreImaging]
      nodAndShuffle <- arbitrary[Option[GmosNodAndShuffle]]
    } yield StaticConfig.GmosSouth(stageMode, detector, mosPreImaging, nodAndShuffle)
  )

  given Cogen[StaticConfig.GmosSouth] =
    Cogen[(GmosSouthStageMode, GmosSouthDetector, MosPreImaging, Option[GmosNodAndShuffle])]
      .contramap(s => (s.stageMode, s.detector, s.mosPreImaging, s.nodAndShuffle))

  given Arbitrary[StaticConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[StaticConfig.GmosNorth],
      arbitrary[StaticConfig.GmosSouth]
    )
  )

  given Cogen[StaticConfig] =
    Cogen[Either[StaticConfig.GmosNorth, StaticConfig.GmosSouth]]
      .contramap {
        case s @ StaticConfig.GmosNorth(_, _, _, _) => s.asLeft
        case s @ StaticConfig.GmosSouth(_, _, _, _) => s.asRight
      }
}

object ArbStaticConfig extends ArbStaticConfig
