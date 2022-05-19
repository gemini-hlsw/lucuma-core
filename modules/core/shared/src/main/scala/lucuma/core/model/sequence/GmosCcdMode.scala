// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.`enum`._
import monocle.Focus
import monocle.Lens

final case class GmosCcdMode(
  xBin:        GmosXBinning,
  yBin:        GmosYBinning,
  ampCount:    GmosAmpCount,
  ampGain:     GmosAmpGain,
  ampReadMode: GmosAmpReadMode
)

object GmosCcdMode {
  implicit val eqGmosCcdMode: Eq[GmosCcdMode] =
    Eq.by(x => (x.xBin, x.yBin, x.ampCount, x.ampGain, x.ampReadMode))

  /** @group Optics */
  val xBin: Lens[GmosCcdMode, GmosXBinning] =
    Focus[GmosCcdMode](_.xBin)

  /** @group Optics */
  val yBin: Lens[GmosCcdMode, GmosYBinning] =
    Focus[GmosCcdMode](_.yBin)

  /** @group Optics */
  val ampCount: Lens[GmosCcdMode, GmosAmpCount] =
    Focus[GmosCcdMode](_.ampCount)

  /** @group Optics */
  val ampGain: Lens[GmosCcdMode, GmosAmpGain] =
    Focus[GmosCcdMode](_.ampGain)

  /** @group Optics */
  val ampReadMode: Lens[GmosCcdMode, GmosAmpReadMode] =
    Focus[GmosCcdMode](_.ampReadMode)
}
