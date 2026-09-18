// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.AltairMode
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

  test("Altair fixes the probe to the one holding its natural guide star, for every GNIRS mode"):
    val gnirs = List(ObservingModeType.GnirsImaging, ObservingModeType.GnirsLongSlit, ObservingModeType.GnirsIfu)
    for
      m <- gnirs
      a <- Enumerated[AltairMode].all
    do
      assertEquals(probes.allowedProbes(m, Some(a)).toList, List(a.guideProbe), clue = s"${m.tag} / ${a.tag}")
      Enumerated[TrackType].all.foreach: t =>
        assertEquals(probes.defaultGuideProbe(m, t, Some(a)), Some(a.guideProbe), clue = s"${m.tag} / ${t.tag}")

  test("LGS+P1 guides on PWFS1, the other Altair modes on the Altair WFS"):
    assertEquals(probes.allowedProbes(ObservingModeType.GnirsLongSlit, Some(AltairMode.LgsP1)).toList, List(GuideProbe.PWFS1))
    assertEquals(probes.allowedProbes(ObservingModeType.GnirsLongSlit, Some(AltairMode.Ngs)).toList, List(GuideProbe.AltairAOWFS))
    assertEquals(probes.allowedProbes(ObservingModeType.GnirsLongSlit, Some(AltairMode.Lgs)).toList, List(GuideProbe.AltairAOWFS))

  test("Altair on a non-GNIRS mode leaves nothing to guide with"):
    modes.filterNot(_.tag.startsWith("gnirs")).foreach: m =>
      assert(probes.allowedProbes(m, Some(AltairMode.Ngs)).isEmpty, clue = m.tag)
      assertEquals(probes.defaultGuideProbe(m, TrackType.Sidereal, Some(AltairMode.Ngs)), None, clue = m.tag)

  test("without Altair the Altair-aware rules agree with the plain ones"):
    for
      m <- modes
      t <- Enumerated[TrackType].all
    do
      assertEquals(probes.allowedProbes(m, None), probes.allowedProbes(m), clue = m.tag)
      assertEquals(probes.defaultGuideProbe(m, t, None), probes.defaultGuideProbe(m, t), clue = m.tag)
