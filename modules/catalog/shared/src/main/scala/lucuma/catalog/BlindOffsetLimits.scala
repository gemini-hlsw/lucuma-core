// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Band
import lucuma.core.enums.Instrument
import lucuma.core.math.BrightnessValue
import lucuma.core.refined.cats.given
import lucuma.core.util.Enumerated

/** Why a blind offset candidate cannot be used with an instrument. */
enum BlindOffsetRejection(val tag: String) derives Enumerated:
  case TooBright           extends BlindOffsetRejection("too_bright")
  case TooFaint            extends BlindOffsetRejection("too_faint")
  case NoMagnitudeEstimate extends BlindOffsetRejection("no_magnitude_estimate")

/**
 * Magnitude limits for blind offset stars with an instrument, in its selection band. A star
 * brighter than `bright` saturates the acquisition image; one fainter than `faint` cannot be
 * centroided. `optimal` is the magnitude penalised least when ranking candidates.
 */
final case class BlindOffsetLimits(
  band:    Band,
  bright:  BrightnessValue,
  faint:   BrightnessValue,
  optimal: BrightnessValue
) derives Eq:

  def rejection(magnitude: Option[BrightnessValue]): Option[BlindOffsetRejection] =
    magnitude match
      case None                                            => Some(BlindOffsetRejection.NoMagnitudeEstimate)
      case Some(m) if m.value.value <= bright.value.value  => Some(BlindOffsetRejection.TooBright)
      case Some(m) if m.value.value >= faint.value.value   => Some(BlindOffsetRejection.TooFaint)
      case _                                               => None

object BlindOffsetLimits:
  private def limits(band: Band, bright: Double, faint: Double, optimal: Double) =
    BlindOffsetLimits(
      band,
      BrightnessValue.unsafeFrom(BigDecimal(bright)),
      BrightnessValue.unsafeFrom(BigDecimal(faint)),
      BrightnessValue.unsafeFrom(BigDecimal(optimal))
    )

  // Bright limits come from ITC full-well predictions at the shortest acquisition exposure under
  // IQ20 CC50. See pyexplore test/blind-offset.py.
  val Alopeke: BlindOffsetLimits    = limits(Band.V, 6.0, 16.0, 8.0)
  val Zorro: BlindOffsetLimits      = limits(Band.V, 6.0, 16.0, 8.0)
  val Ghost: BlindOffsetLimits      = limits(Band.V, 6.0, 18.0, 8.0)
  val Gmos: BlindOffsetLimits       = limits(Band.V, 12.4, 19.0, 13.0)
  val Flamingos2: BlindOffsetLimits = limits(Band.H, 13.0, 18.0, 14.0)
  val Gnirs: BlindOffsetLimits      = limits(Band.H, 12.0, 17.0, 13.0)
  val Igrins2: BlindOffsetLimits    = limits(Band.H, 13.0, 18.0, 14.0)

  /** Limits for an instrument, or `None` when it does not support blind offsets. */
  def forInstrument(instrument: Instrument): Option[BlindOffsetLimits] =
    instrument match
      case Instrument.Alopeke                           => Some(Alopeke)
      case Instrument.Zorro                             => Some(Zorro)
      case Instrument.Ghost                             => Some(Ghost)
      case Instrument.GmosNorth | Instrument.GmosSouth  => Some(Gmos)
      case Instrument.Flamingos2                        => Some(Flamingos2)
      case Instrument.Gnirs                             => Some(Gnirs)
      case Instrument.Igrins2                           => Some(Igrins2)
      case Instrument.MaroonX                           => None
      case Instrument.AcqCamNorth | Instrument.AcqCamSouth | Instrument.Gpi | Instrument.Gsaoi |
          Instrument.Niri | Instrument.Scorpio | Instrument.VisitorNorth |
          Instrument.VisitorSouth =>
        None
