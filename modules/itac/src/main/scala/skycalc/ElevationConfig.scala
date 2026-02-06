// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc;

import lucuma.core.enums.TwilightType

final case class ElevationConfig(bounds: TwilightType, minAirmass: Double, maxAirmass: Double)

object ElevationConfig:
  val DEFAULT = ElevationConfig(TwilightType.Nautical, 1.00, 2.15);
