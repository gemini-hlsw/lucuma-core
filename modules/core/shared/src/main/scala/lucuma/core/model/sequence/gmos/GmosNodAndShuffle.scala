// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Eq
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GmosEOffsetting
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

final case class GmosNodAndShuffle(
  posA:          Offset,
  posB:          Offset,
  eOffset:       GmosEOffsetting,
  shuffleOffset: PosInt,
  shuffleCycles: PosInt
)

object GmosNodAndShuffle {
  given Eq[GmosNodAndShuffle] =
    Eq.by(x => (x.posA, x.posB, x.eOffset, x.shuffleOffset, x.shuffleCycles))

  /** @group Optics */
  val posA: Lens[GmosNodAndShuffle, Offset] =
    Focus[GmosNodAndShuffle](_.posA)

  /** @group Optics */
  val posB: Lens[GmosNodAndShuffle, Offset] =
    Focus[GmosNodAndShuffle](_.posB)

  /** @group Optics */
  val eOffset: Lens[GmosNodAndShuffle, GmosEOffsetting] =
    Focus[GmosNodAndShuffle](_.eOffset)

  /** @group Optics */
  val shuffleOffset: Lens[GmosNodAndShuffle, PosInt] =
    Focus[GmosNodAndShuffle](_.shuffleOffset)

  /** @group Optics */
  val shuffleCycles: Lens[GmosNodAndShuffle, PosInt] =
    Focus[GmosNodAndShuffle](_.shuffleCycles)
}
