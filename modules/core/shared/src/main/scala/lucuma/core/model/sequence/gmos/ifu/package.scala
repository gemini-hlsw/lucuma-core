// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package ifu

import cats.data.NonEmptyList
import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.TelescopeConfigSpec

/**
  * Optimal GMOS binning calculation for IFU (Integral Field Unit) mode.
  * IFU observations always use 1x1 binning to maintain spatial resolution
  * required for proper reconstruction of the integral field.
  */
private val Binning: (GmosXBinning, GmosYBinning) =
  (GmosXBinning.One, GmosYBinning.One)

val NorthBinning: (GmosXBinning, GmosYBinning) =
  Binning

val SouthBinning: (GmosXBinning, GmosYBinning) =
  Binning

// GMOS IFU telescope-config presets. The head of each list is the default
// used to initialize observing modes at creation -- for GMOS IFU that is "no offsets".
private def presets(
  p: BigDecimal,
  q: BigDecimal
): NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  def at(dp: BigDecimal, dq: BigDecimal): TelescopeConfigSpec =
    TelescopeConfigSpec.objectAt(Offset(dp.pArcsec, dq.qArcsec))

  NonEmptyList.of(
    "No offsets" -> NonEmptyList.one(TelescopeConfigSpec.objectAt(Offset.Zero)),
    "On-source"  -> NonEmptyList.of(at(p, q), at(p, -q), at(-p, -q), at(-p, q))
  )

private val OneSlitPresets: NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  presets(BigDecimal("0.2"), BigDecimal("0.5"))

private val TwoSlitsPresets: NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  presets(BigDecimal("1.5"), BigDecimal("0.9"))

def northTelescopeConfigPresets(
  fpu: GmosNorthIfuFpu
): NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  fpu match
    case GmosNorthIfuFpu.TwoSlits                                 => TwoSlitsPresets
    case GmosNorthIfuFpu.OneSlitRed | GmosNorthIfuFpu.OneSlitBlue => OneSlitPresets

def southTelescopeConfigPresets(
  fpu: GmosSouthIfuFpu
): NonEmptyList[(String, NonEmptyList[TelescopeConfigSpec])] =
  fpu match
    case GmosSouthIfuFpu.TwoSlits                                 => TwoSlitsPresets
    case GmosSouthIfuFpu.OneSlitRed | GmosSouthIfuFpu.OneSlitBlue => OneSlitPresets

/** The value observing modes are created with: a single guided position on target. */
val DefaultTelescopeConfigs: NonEmptyList[TelescopeConfigSpec] =
  OneSlitPresets.head._2
