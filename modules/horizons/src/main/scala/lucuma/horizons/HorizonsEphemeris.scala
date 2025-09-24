// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.AirMass
import lucuma.core.model.EphemerisKey
import lucuma.core.model.Extinction

import java.time.Instant

final case class HorizonsEphemeris(
  key: EphemerisKey.Horizons,
  site: Site,
  start: Instant,
  stop: Instant,
  entries: List[HorizonsEphemerisEntry]
)

final case class HorizonsEphemerisEntry(
  when: Instant,
  coordinates: Coordinates,
  velocity: Offset,
  airmass: Option[AirMass],
  extinction: Option[Extinction],
  visualMagnitude: Double,
  surfaceBrightness: Option[Double],
)