// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Axis along which an instrument spreads a spectrum, in pre-image detector coordinates.
 *
 * This decides how a mask file's x and y slit columns are to be read: the slit's ''width'' is
 * always its extent along the dispersion direction, and its ''length'' the extent perpendicular to
 * it. GMOS disperses horizontally, so its x column is the width; Flamingos-2 disperses vertically,
 * so its x column is the length.
 *
 * Tags are the values of the `DISPDIR` keyword in a MOS mask file.
 *
 * @group Enumerations
 */
enum MosDispersionDirection(
  val tag:       String,
  val shortName: String,
  val longName:  String
) derives Enumerated:

  /** GMOS-N and GMOS-S. The x axis is the dispersion axis. */
  case Horizontal extends MosDispersionDirection("horizontal", "X", "Horizontal")

  /** Flamingos-2. The y axis is the dispersion axis. */
  case Vertical   extends MosDispersionDirection("vertical",   "Y", "Vertical")

object MosDispersionDirection:

  given Display[MosDispersionDirection] =
    Display.by(_.shortName, _.longName)
