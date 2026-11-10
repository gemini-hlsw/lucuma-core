// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.Eq
import cats.derived.*
import cats.syntax.order.*
import lucuma.core.enums.Flamingos2AcquisitionType
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Iso
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

/**
 * Flamingos 2 has 2 acquisition sequence generation modes: Bright and Faint.
 * Faint mode subtracts a sky frame, taken at its sky offset.
 */
enum Flamingos2AcquisitionMode(val acquisitionType: Flamingos2AcquisitionType) derives Eq:
  case Bright extends Flamingos2AcquisitionMode(Flamingos2AcquisitionType.Bright)
  case Faint(skyOffset: Offset) extends Flamingos2AcquisitionMode(Flamingos2AcquisitionType.Faint)

object Flamingos2AcquisitionMode:
  object Faint:
    val DefaultSkyOffset: Offset = Offset(0.pArcsec, 10.qArcsec)

    val Default: Flamingos2AcquisitionMode.Faint =
      Flamingos2AcquisitionMode.Faint(DefaultSkyOffset)

    val skyOffset: Iso[Flamingos2AcquisitionMode.Faint, Offset] =
      Focus[Flamingos2AcquisitionMode.Faint](_.skyOffset)

  val bright: Prism[Flamingos2AcquisitionMode, Flamingos2AcquisitionMode.Bright.type] =
    GenPrism[Flamingos2AcquisitionMode, Flamingos2AcquisitionMode.Bright.type]

  val faint: Prism[Flamingos2AcquisitionMode, Flamingos2AcquisitionMode.Faint] =
    GenPrism[Flamingos2AcquisitionMode, Flamingos2AcquisitionMode.Faint]

  val skyOffset: Optional[Flamingos2AcquisitionMode, Offset] =
    faint.andThen(Faint.skyOffset)

  /** The longest acquisition exposure taken without a sky frame. */
  val BrightExposureLimit: TimeSpan = 2.secondTimeSpan

  /**
   * The mode used when none is chosen explicitly: Bright when the exposure the
   * target needs is at most `BrightExposureLimit`, Faint otherwise.
   */
  def defaultFor(exposureTime: TimeSpan): Flamingos2AcquisitionMode =
    if exposureTime <= BrightExposureLimit then Bright else Faint.Default

  /** The mode for an acquisition type, using the given sky offset if that type is Faint. */
  def forTypeAndOffset(acquisitionType: Flamingos2AcquisitionType, skyOffset: Offset): Flamingos2AcquisitionMode =
    acquisitionType match
      case Flamingos2AcquisitionType.Bright => Bright
      case Flamingos2AcquisitionType.Faint  => Faint(skyOffset)
