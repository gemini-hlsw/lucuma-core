// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Common interface for an instrument's slit offset presets.
 * Each instrument defines its own enum of presets
 */
sealed trait SlitOffsetPreset:
  def tag: String
  def description: String

/**
 * Enumerated type for the Flamingos2 slit offset presets.
 */
enum Flamingos2SlitOffsetPreset(
  val tag:         String,
  val description: String
) extends SlitOffsetPreset derives Enumerated, Display:
  case Telluric     extends Flamingos2SlitOffsetPreset("telluric",       "Telluric")
  case NodAlongSlit extends Flamingos2SlitOffsetPreset("nod_along_slit", "Nod along slit")
  case NodToSky     extends Flamingos2SlitOffsetPreset("nod_to_sky",     "Nod to sky")

/**
 * Enumerated type for the GNIRS slit offset presets.
 */
enum GnirsSlitOffsetPreset(
  val tag:         String,
  val description: String
) extends SlitOffsetPreset derives Enumerated, Display:
  case NodAlongSlit extends GnirsSlitOffsetPreset("nod_along_slit", "Nod along slit")
  case NodToSky     extends GnirsSlitOffsetPreset("nod_to_sky",     "Nod to sky")

/**
 * Enumerated type for the IGRINS2 slit offset presets.
 */
enum Igrins2SlitOffsetPreset(
  val tag:         String,
  val description: String
) extends SlitOffsetPreset derives Enumerated, Display:
  case NodAlongSlit extends Igrins2SlitOffsetPreset("nod_along_slit", "Nod along slit")
  case NodToSky     extends Igrins2SlitOffsetPreset("nod_to_sky",     "Nod to sky")
