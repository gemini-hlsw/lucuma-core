// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.scorpio

import cats.Eq
import cats.derived.*
import lucuma.core.enums.ScorpioFpu
import monocle.Prism

/**
 * SCORPIO focal plane unit state for a step. The cold block is used for IR darks, since the IR
 * detectors have no shutter.
 */
enum ScorpioFpuMask derives Eq:
  case Imaging
  case LongSlit(fpu: ScorpioFpu)
  case ColdBlock

  def longSlit: Option[ScorpioFpu] =
    this match
      case LongSlit(f) => Some(f)
      case _           => None

object ScorpioFpuMask:
  /** @group Optics */
  val longSlit: Prism[ScorpioFpuMask, ScorpioFpu] =
    Prism[ScorpioFpuMask, ScorpioFpu](_.longSlit)(LongSlit(_))
