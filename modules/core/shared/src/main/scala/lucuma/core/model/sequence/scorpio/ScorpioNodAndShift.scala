// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

/**
 * VIS Nod-and-Shift. As with GMOS nod-and-shuffle, a single step runs the whole cycle: the
 * telescope nods between A and B while the VIS charge is shifted by `shiftOffset` rows, and the
 * CCDs are read once at the end.
 */
final case class ScorpioNodAndShift(
  posA:        Offset,
  posB:        Offset,
  shiftOffset: PosInt,
  shiftCycles: PosInt
) derives Eq

object ScorpioNodAndShift:
  /** @group Optics */
  val posA: Lens[ScorpioNodAndShift, Offset] =
    Focus[ScorpioNodAndShift](_.posA)

  /** @group Optics */
  val posB: Lens[ScorpioNodAndShift, Offset] =
    Focus[ScorpioNodAndShift](_.posB)

  /** @group Optics */
  val shiftOffset: Lens[ScorpioNodAndShift, PosInt] =
    Focus[ScorpioNodAndShift](_.shiftOffset)

  /** @group Optics */
  val shiftCycles: Lens[ScorpioNodAndShift, PosInt] =
    Focus[ScorpioNodAndShift](_.shiftCycles)
