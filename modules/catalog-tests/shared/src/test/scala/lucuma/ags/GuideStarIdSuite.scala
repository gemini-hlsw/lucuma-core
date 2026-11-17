// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.kernel.laws.discipline.OrderTests
import lucuma.ags.arb.ArbGuideStarId.given
import lucuma.core.model.Target
import org.scalacheck.Prop.forAll

class GuideStarIdSuite extends munit.DisciplineSuite:
  private val T1: GuideStarId   = GuideStarId.ObservationTarget(Target.Id.fromLong(1L))
  private val T2: GuideStarId   = GuideStarId.ObservationTarget(Target.Id.fromLong(2L))
  private val NoId: GuideStarId = GuideStarId.ObservationTarget(None)
  private val G1: GuideStarId   = GuideStarId.GaiaDr3(1L)
  private val G2: GuideStarId   = GuideStarId.GaiaDr3(2L)

  checkAll("GuideStarId", OrderTests[GuideStarId].order)

  property("any observation target sorts before any catalog star"):
    forAll: (target: GuideStarId, star: GuideStarId) =>
      !target.isObservationTarget || star.isObservationTarget || Order[GuideStarId].lt(target, star)

  test("catalog stars sort numerically by Gaia source id"):
    val ids: List[GuideStarId] =
      List(10L, 9L, 0L, -1L, -10L, 100L, Long.MaxValue, Long.MinValue).map(GuideStarId.GaiaDr3(_))
    assertEquals(
      ids.sorted(using Order[GuideStarId].toOrdering).map(_.gaiaSourceId),
      List(Long.MinValue, -10L, -1L, 0L, 9L, 10L, 100L, Long.MaxValue).map(Option.apply)
    )
    assert(Order[GuideStarId].lt(GuideStarId.GaiaDr3(9L), GuideStarId.GaiaDr3(10L)))
    assert(Order[GuideStarId].lt(G1, G2))

  test("observation targets sort by target id, the id-less one first"):
    assert(Order[GuideStarId].lt(NoId, T1))
    assert(Order[GuideStarId].lt(T1, T2))

  test("selects"):
    val selections: List[(GuideStarId, GuideStarId, Boolean)] =
      List(
        (G1, G1, true),
        (G1, G2, false),
        (G1, T1, false),
        (G1, NoId, false),
        (NoId, T1, true),
        (NoId, T2, true),
        (NoId, NoId, true),
        (NoId, G1, false),
        (T1, T1, true),
        (T1, T2, false),
        (T1, NoId, false),
        (T1, G1, false)
      )
    selections.foreach: (selection, candidate, expected) =>
      assertEquals(selection.selects(candidate), expected, s"$selection selects $candidate")

  test("observationTargetId and isObservationTarget"):
    assertEquals(T1.observationTargetId, Target.Id.fromLong(1L))
    assertEquals(NoId.observationTargetId, None)
    assertEquals(G1.observationTargetId, None)
    assertEquals(List(T1, NoId, G1).map(_.isObservationTarget), List(true, true, false))

  test("catalogName"):
    assertEquals(GuideStarId.GaiaDr3(123L).catalogName.map(_.value.value), Some("Gaia DR3 123"))
    assertEquals(GuideStarId.GaiaDr3(-5L).catalogName.map(_.value.value), Some("Gaia DR3 -5"))
    assertEquals(T1.catalogName, None)
    assertEquals(NoId.catalogName, None)

  test("gaiaSourceId"):
    assertEquals(GuideStarId.GaiaDr3(123L).gaiaSourceId, Some(123L))
    assertEquals(T1.gaiaSourceId, None)
    assertEquals(NoId.gaiaSourceId, None)
