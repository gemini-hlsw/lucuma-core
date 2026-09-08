// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.TrackType
import lucuma.core.util.Enumerated
import munit.FunSuite

final class ProbesSuite extends FunSuite:

  private val modes = Enumerated[ObservingModeType].all

  test("every observing mode has an allowed set (no MatchError)"):
    modes.foreach(m => probes.allowedProbes(m))

  test("only exchange modes have no allowed probe"):
    modes.foreach: m =>
      val empty = probes.allowedProbes(m).isEmpty
      assertEquals(empty, m.isInstanceOf[ExchangeObservingModeType], clue = m.tag)

  test("an OIWFS is never selected for a nonsidereal target"):
    modes.foreach: m =>
      probes.defaultGuideProbe(m, TrackType.Nonsidereal).foreach: p =>
        assertEquals(p, GuideProbe.PWFS2, clue = m.tag)

  test("the automatically selected probe is always an allowed one"):
    for
      m <- modes
      t <- Enumerated[TrackType].all
      p <- probes.defaultGuideProbe(m, t)
    do assert(probes.isProbeAllowed(m, p), s"${m.tag} / ${t.tag}: $p not allowed")

  test("GHOST allows both PWFS, defaulting to PWFS2"):
    assertEquals(
      probes.allowedProbes(ObservingModeType.GhostIfu).toList,
      List(GuideProbe.PWFS2, GuideProbe.PWFS1)
    )

  test("allowed probes come back best-first"):
    assertEquals(
      probes.allowedProbes(ObservingModeType.GmosNorthLongSlit).toList,
      List(GuideProbe.GmosOIWFS, GuideProbe.PWFS2, GuideProbe.PWFS1)
    )
    assertEquals(
      probes.allowedProbes(ObservingModeType.Flamingos2LongSlit).toList,
      List(GuideProbe.Flamingos2OIWFS, GuideProbe.PWFS2, GuideProbe.PWFS1)
    )
    assertEquals(
      probes.allowedProbes(ObservingModeType.GnirsLongSlit).toList,
      List(GuideProbe.PWFS2, GuideProbe.PWFS1)
    )
