// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package ifu

import cats.data.NonEmptyList
import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfig

/**
  * Optimal GMOS binning calculation for IFU (Integral Field Unit) mode.
  * IFU observations always use 1x1 binning to maintain spatial resolution
  * required for proper reconstruction of the integral field.
  */
val ifuBinning: (GmosXBinning, GmosYBinning) =
  (GmosXBinning.One, GmosYBinning.One)

val northBinning: (GmosXBinning, GmosYBinning) =
  ifuBinning

val southBinning: (GmosXBinning, GmosYBinning) =
  ifuBinning

// GMOS IFU telescope-config presets. The head of each list is the default
// used to initialize observing modes at creation -- for GMOS IFU that is "no offsets".
private def presets(
  p: BigDecimal,
  q: BigDecimal
): NonEmptyList[(String, NonEmptyList[TelescopeConfig])] =
  def at(dp: BigDecimal, dq: BigDecimal): TelescopeConfig =
    TelescopeConfig(Offset(dp.pArcsec, dq.qArcsec), StepGuideState.Enabled)

  NonEmptyList.of(
    "No offsets" -> NonEmptyList.one(TelescopeConfig(Offset.Zero, StepGuideState.Enabled)),
    "On-source"  -> NonEmptyList.of(at(p, q), at(p, -q), at(-p, -q), at(-p, q))
  )

private val OneSlitPresets: NonEmptyList[(String, NonEmptyList[TelescopeConfig])] =
  presets(BigDecimal("0.2"), BigDecimal("0.5"))

private val TwoSlitsPresets: NonEmptyList[(String, NonEmptyList[TelescopeConfig])] =
  presets(BigDecimal("1.5"), BigDecimal("0.9"))

def northIfuTelescopeConfigPresets(
  fpu: GmosNorthIfuFpu
): NonEmptyList[(String, NonEmptyList[TelescopeConfig])] =
  fpu match
    case GmosNorthIfuFpu.TwoSlits                                 => TwoSlitsPresets
    case GmosNorthIfuFpu.OneSlitRed | GmosNorthIfuFpu.OneSlitBlue => OneSlitPresets

def southIfuTelescopeConfigPresets(
  fpu: GmosSouthIfuFpu
): NonEmptyList[(String, NonEmptyList[TelescopeConfig])] =
  fpu match
    case GmosSouthIfuFpu.TwoSlits                                 => TwoSlitsPresets
    case GmosSouthIfuFpu.OneSlitRed | GmosSouthIfuFpu.OneSlitBlue => OneSlitPresets

/** The value observing modes are created with: a single guided position on target. */
val DefaultTelescopeConfigs: NonEmptyList[TelescopeConfig] =
  OneSlitPresets.head._2
