// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import coulomb._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enums.*
import lucuma.core.math.units.Electron
import monocle.Focus
import monocle.Lens

final case class GmosCcdMode(
  xBin:        GmosXBinning,
  yBin:        GmosYBinning,
  ampCount:    GmosAmpCount,
  ampGain:     GmosAmpGain,
  ampReadMode: GmosAmpReadMode
) {

  def gmosNorthReadNoise: Quantity[PosBigDecimal, Electron] = {
    val bd = (ampReadMode, ampGain) match {
      case (GmosAmpReadMode.Slow, GmosAmpGain.Low ) => BigDecimal(414L, 2)
      case (GmosAmpReadMode.Slow, GmosAmpGain.High) => BigDecimal(480L, 2)
      case (GmosAmpReadMode.Fast, GmosAmpGain.Low ) => BigDecimal(627L, 2)
      case (GmosAmpReadMode.Fast, GmosAmpGain.High) => BigDecimal(869L, 2)
    }

    PosBigDecimal.unsafeFrom(bd).withUnit[Electron]
  }

  def gmosSouthReadNoise: Quantity[PosBigDecimal, Electron] = {
    val bd = (ampReadMode, ampGain) match {
      case (GmosAmpReadMode.Slow, GmosAmpGain.Low ) => BigDecimal(400L, 2)
      case (GmosAmpReadMode.Slow, GmosAmpGain.High) => BigDecimal(480L, 2)
      case (GmosAmpReadMode.Fast, GmosAmpGain.Low ) => BigDecimal(660L, 2)
      case (GmosAmpReadMode.Fast, GmosAmpGain.High) => BigDecimal(790L, 2)
    }

    PosBigDecimal.unsafeFrom(bd).withUnit[Electron]
  }

  def readNoise(site: Site): Quantity[PosBigDecimal, Electron] =
    site match {
      case Site.GN => gmosNorthReadNoise
      case Site.GS => gmosSouthReadNoise
    }

}

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
