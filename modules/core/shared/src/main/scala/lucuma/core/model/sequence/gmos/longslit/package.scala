// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos.longslit

import cats.Order
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthDetector
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthDetector
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.model.SourceProfile

val DefaultAmpReadMode: GmosAmpReadMode =
  GmosAmpReadMode.Slow

val DefaultAmpGain: GmosAmpGain =
  GmosAmpGain.Low

val DefaultRoi: GmosRoi =
  GmosRoi.FullFrame

val DefaultYBinning: GmosYBinning =
  GmosYBinning.Two

val DefaultAmpCount: GmosAmpCount =
  GmosAmpCount.Twelve

private given Order[Angle] =
  Angle.AngleOrder

private val DescendingXBinning: List[GmosXBinning] =
  GmosXBinning.all.sortBy(b => -b.count)

/**
  * Object angular size estimate based on source profile alone.
  */
def objectSize(p: SourceProfile): Angle =
  p match {
    case SourceProfile.Point(_)          => Angle.Angle0
    case SourceProfile.Uniform(_)        => Angle.Angle180
    case SourceProfile.Gaussian(fwhm, _) => fwhm
  }

/**
  * Effective size of a target with the given source profile and image quality.
  */
def effectiveSize(p: SourceProfile, iq: ImageQuality): Angle =
  objectSize(p) max iq.toAngle

def effectiveSlitWidth(p: SourceProfile, iq: ImageQuality, slitWidth: Angle): Angle =
  slitWidth min effectiveSize(p, iq)

def pixelSize(site: Site): Angle =
  site match {
      case Site.GN => GmosNorthDetector.Hamamatsu.pixelSize
      case Site.GS => GmosSouthDetector.Hamamatsu.pixelSize
    }

private def xbin(site: Site, slitWidth: Angle, sampling: PosDouble): GmosXBinning = {
  val npix = slitWidth.toMicroarcseconds.toDouble / pixelSize(site).toMicroarcseconds.toDouble
  DescendingXBinning.find(b => npix / b.count.toDouble >= sampling.value).getOrElse(GmosXBinning.One)
}

/**
* Calculates the best `GmosXBinning` value to use for GMOS North long slit observing for
* the desired sampling.
*
* @param fpu      GMOS North FPU
* @param p        SourceProfile of the target
* @param iq       expected/required ImageQuality
* @param sampling desired sampling rate
*/
def xbinNorth(fpu: GmosNorthFpu, p: SourceProfile, iq: ImageQuality, sampling: PosDouble): GmosXBinning =
  xbin(Site.GN, fpu.effectiveSlitWidth min effectiveSize(p, iq), sampling)

/**
* Calculates the best `GmosXBinning` value to use for GMOS South long slit observing for
* the desired sampling.
*
* @param fpu      GMOS South FPU
* @param p        SourceProfile of the target
* @param iq       expected/required ImageQuality
* @param sampling desired sampling rate
*/
def xbinSouth(fpu: GmosSouthFpu, p: SourceProfile, iq: ImageQuality, sampling: PosDouble): GmosXBinning =
  xbin(Site.GS, fpu.effectiveSlitWidth min effectiveSize(p, iq), sampling)

