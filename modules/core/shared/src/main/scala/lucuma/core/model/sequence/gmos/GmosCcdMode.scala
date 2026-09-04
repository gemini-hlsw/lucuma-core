// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Order
import cats.data.NonEmptyList
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Electron
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import monocle.Focus
import monocle.Lens
import spire.math.Rational

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
  // For multiple targets, we take the smallest binning for each axis.
  // https://docs.google.com/document/d/1P8_pXLRVomUSvofyVkAniOyGROcAtiJ7EMYt9wWXB0o/edit?disco=AAAA32SmtD4
  private def asterismBinning(bs: NonEmptyList[(GmosXBinning, GmosYBinning)]): (GmosXBinning, GmosYBinning) =
    (bs.map(_._1).minimumBy(_.count), bs.map(_._2).minimumBy(_.count))

  object Default {

    private def defaultCcdMode(xBinning: GmosXBinning, yBinning: GmosYBinning): GmosCcdMode =
      GmosCcdMode(
        xBinning,
        yBinning,
        longslit.DefaultAmpCount,
        longslit.DefaultAmpGain,
        longslit.DefaultAmpReadMode
      )

    object Longslit:

      def gmosNorth(
        profiles:     NonEmptyList[SourceProfile],
        fpu:          GmosNorthFpu,
        grating:      GmosNorthGrating,
        imageQuality: ImageQuality
      ): GmosCcdMode =
        val (defaultXBinning, defaultYBinning) =
          if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
          else asterismBinning(profiles.map(longslit.northBinning(fpu, _, imageQuality, grating)))

        defaultCcdMode(defaultXBinning, defaultYBinning)

      def gmosSouth(
        profiles:     NonEmptyList[SourceProfile],
        fpu:          GmosSouthFpu,
        grating:      GmosSouthGrating,
        imageQuality: ImageQuality
      ): GmosCcdMode =
        val (defaultXBinning, defaultYBinning) =
          if (fpu.isIFU) (GmosXBinning.One, GmosYBinning.One)
          else asterismBinning(profiles.map(longslit.southBinning(fpu, _, imageQuality, grating)))

        defaultCcdMode(defaultXBinning, defaultYBinning)

    /**
     * Default MOS CCD mode for a set of source profiles, ported back from explore.
     */
    object Mos:

      def apply(
        profiles:   NonEmptyList[SourceProfile],
        slitWidth:  Angle,
        iq:         ImageQuality,
        dispersion: Quantity[Rational, NanometersPerPixel],
        resolution: PosInt,
        blaze:      Wavelength,
        pixelScale: Angle
      ): GmosCcdMode =
        val (xBinning, yBinning) =
          asterismBinning(profiles.map(mos.mosBinning(slitWidth, _, iq, dispersion, resolution, blaze, pixelScale)))
        defaultCcdMode(xBinning, yBinning)
  }

  given Order[GmosCcdMode] =
    Order.by { a => (
      a.xBin,
      a.yBin,
      a.ampCount,
      a.ampGain,
      a.ampReadMode
    )}

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
