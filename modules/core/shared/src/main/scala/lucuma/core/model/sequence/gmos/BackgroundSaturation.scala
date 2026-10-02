// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.SkyBackground
import lucuma.core.util.TimeSpan

/**
 * Exposure times at which the sky background saturates the GMOS Hamamatsu
 * detectors when imaging unbinned at low gain, by filter and sky background.
 * Ported from the OCS p2checker `GmosRule` (REL-3057).  The GMOS-N values
 * there were half full well times, doubled; the GMOS-S values were saturation
 * times already.  A filter missing here is simply not checked, so adding a
 * row is all it takes to check another filter.
 */
object BackgroundSaturation:

  type Row = Map[SkyBackground, TimeSpan]

  // Seconds, in sky background order: SB20, SB50, SB80, SBAny.
  private def row(sb20: Long, sb50: Long, sb80: Long, sbAny: Long): Row =
    Map(
      SkyBackground.Darkest -> TimeSpan.unsafeFromMicroseconds(sb20  * 1_000_000L),
      SkyBackground.Dark    -> TimeSpan.unsafeFromMicroseconds(sb50  * 1_000_000L),
      SkyBackground.Gray    -> TimeSpan.unsafeFromMicroseconds(sb80  * 1_000_000L),
      SkyBackground.Bright  -> TimeSpan.unsafeFromMicroseconds(sbAny * 1_000_000L)
    )

  val North: Map[GmosNorthFilter, Row] =
    Map(
      GmosNorthFilter.GPrime -> row(24336, 10008, 3000,  420),
      GmosNorthFilter.RPrime -> row(11592,  6396, 2604,  456),
      GmosNorthFilter.IPrime -> row( 6396,  4200, 2304,  564),
      GmosNorthFilter.ZPrime -> row( 1284,  1236, 1200,  900),
      GmosNorthFilter.Z      -> row( 3600,  3204, 2664, 1380),
      GmosNorthFilter.Y      -> row( 5304,  5304, 5304, 5304)
    )

  val South: Map[GmosSouthFilter, Row] =
    Map(
      GmosSouthFilter.GPrime -> row(15660,  6588, 1950,  270),
      GmosSouthFilter.RPrime -> row( 6588,  3672, 1500,  258),
      GmosSouthFilter.IPrime -> row( 3780,  2460, 1338,  330),
      GmosSouthFilter.ZPrime -> row(  738,   720,  690,  522),
      GmosSouthFilter.Z      -> row( 2100,  1866, 1548,  798),
      GmosSouthFilter.Y      -> row( 3156,  3162, 3168, 3168)
    )
