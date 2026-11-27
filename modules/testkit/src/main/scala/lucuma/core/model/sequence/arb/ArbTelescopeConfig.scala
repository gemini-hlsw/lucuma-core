// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbTelescopeConfig:
  import ArbEnumerated.given
  import ArbOffset.given

  given Arbitrary[TelescopeConfig] =
    Arbitrary:
      for
        o <- arbitrary[Offset]
        g <- arbitrary[StepGuideState]
        p <- arbitrary[OffsetPurpose]
      yield TelescopeConfig(o, g, p)

  given Cogen[TelescopeConfig] =
    Cogen[(
      Offset,
      StepGuideState,
      OffsetPurpose
    )].contramap: a =>
      (a.offset, a.guiding, a.purpose)

object ArbTelescopeConfig extends ArbTelescopeConfig
