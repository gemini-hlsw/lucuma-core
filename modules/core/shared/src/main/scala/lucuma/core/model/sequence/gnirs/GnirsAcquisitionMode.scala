// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Iso
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

/**
  * Gnirs has 3 acquisition sequence generation modes: Very Bright, Bright and Faint.
  * Faint mode also requires a sky offset, which the other 2 modes don't.
  */
enum GnirsAcquisitionMode(val acquisitionType: GnirsAcquisitionType) derives Eq:
  case VeryBright extends GnirsAcquisitionMode(GnirsAcquisitionType.VeryBright)
  case Bright extends GnirsAcquisitionMode(GnirsAcquisitionType.Bright)
  case Faint(skyOffset: Offset) extends GnirsAcquisitionMode(GnirsAcquisitionType.Faint)

object GnirsAcquisitionMode:
  object Faint {
    val DefaultSkyOffset: Offset = Offset(0.pArcsec, -2.qArcsec)

    val Default: GnirsAcquisitionMode.Faint =
      GnirsAcquisitionMode.Faint(DefaultSkyOffset)

    val skyOffset: Iso[GnirsAcquisitionMode.Faint, Offset] = Focus[GnirsAcquisitionMode.Faint](_.skyOffset)
  }

  val veryBright: Prism[GnirsAcquisitionMode, GnirsAcquisitionMode.VeryBright.type] = 
    GenPrism[GnirsAcquisitionMode, GnirsAcquisitionMode.VeryBright.type]

  val bright: Prism[GnirsAcquisitionMode, GnirsAcquisitionMode.Bright.type] = 
    GenPrism[GnirsAcquisitionMode, GnirsAcquisitionMode.Bright.type]

  val faint: Prism[GnirsAcquisitionMode, GnirsAcquisitionMode.Faint] = 
    GenPrism[GnirsAcquisitionMode, GnirsAcquisitionMode.Faint]

  val skyOffset: Optional[GnirsAcquisitionMode, Offset] =
    faint.andThen(Faint.skyOffset)

  def forTypeAndOffset(acquisitionType: GnirsAcquisitionType, skyOffset: Option[Offset]): GnirsAcquisitionMode =
    acquisitionType match
      case GnirsAcquisitionType.VeryBright => VeryBright
      case GnirsAcquisitionType.Bright => Bright
      case GnirsAcquisitionType.Faint => Faint(skyOffset.getOrElse(Faint.DefaultSkyOffset))

  // Default value depends on integration time (exposure time * coadds).
  def defaultFor(exposureTime: TimeSpan, coadds: PosInt): GnirsAcquisitionMode =
    val integrationTimeSeconds: BigDecimal = exposureTime.toSeconds * coadds.value
    if (integrationTimeSeconds < 0.5) GnirsAcquisitionMode.VeryBright
    else if (integrationTimeSeconds < 10) GnirsAcquisitionMode.Bright
    else GnirsAcquisitionMode.Faint.Default
