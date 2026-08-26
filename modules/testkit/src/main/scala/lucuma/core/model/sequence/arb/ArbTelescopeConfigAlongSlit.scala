// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbTelescopeConfigAlongSlit:
  import ArbEnumerated.given
  import ArbOffset.given

  given Arbitrary[TelescopeConfigAlongSlit] =
    Arbitrary:
      for
        o <- arbitrary[Offset.Q]
        g <- arbitrary[StepGuideState]
      yield TelescopeConfigAlongSlit(o, g)

  given Cogen[TelescopeConfigAlongSlit] =
    Cogen[(Offset.Q, StepGuideState)].contramap: a =>
      (a.offset, a.guiding)

object ArbTelescopeConfigAlongSlit extends ArbTelescopeConfigAlongSlit
