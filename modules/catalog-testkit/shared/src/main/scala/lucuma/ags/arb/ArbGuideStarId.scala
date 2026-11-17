// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags.arb

import lucuma.ags.GuideStarId
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbGid.given
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGuideStarId:

  given Arbitrary[GuideStarId] =
    Arbitrary:
      Gen.frequency(
        1 -> Gen.const(GuideStarId.ObservationTarget(None)),
        2 -> Arbitrary
          .arbitrary[Target.Id]
          .map(targetId => GuideStarId.ObservationTarget(Some(targetId))),
        4 -> Arbitrary.arbitrary[Long].map(GuideStarId.GaiaDr3(_))
      )

  given Cogen[GuideStarId] =
    Cogen[(Option[Long], Option[Target.Id], Boolean)].contramap: id =>
      (id.gaiaSourceId, id.observationTargetId, id.isObservationTarget)

object ArbGuideStarId extends ArbGuideStarId
