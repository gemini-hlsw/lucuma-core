// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import lucuma.core.enums.GhostIfu1FiberAgitator
import lucuma.core.enums.GhostIfu2FiberAgitator
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen


trait ArbGhostDynamicConfig:

  import ArbEnumerated.given
  import ArbGhostDetector.given

  given Arbitrary[GhostDynamicConfig] =
    Arbitrary:
      for
        r  <- arbitrary[GhostDetector.Red]
        b  <- arbitrary[GhostDetector.Blue]
        a1 <- arbitrary[GhostIfu1FiberAgitator]
        a2 <- arbitrary[GhostIfu2FiberAgitator]
      yield GhostDynamicConfig(r, b, a1, a2)

  given Cogen[GhostDynamicConfig] =
    Cogen[(
      GhostDetector.Red,
      GhostDetector.Blue,
      GhostIfu1FiberAgitator,
      GhostIfu2FiberAgitator
    )].contramap: a =>
      (
        a.red,
        a.blue,
        a.ifu1FiberAgitator,
        a.ifu2FiberAgitator
      )

object ArbGhostDynamicConfig extends ArbGhostDynamicConfig