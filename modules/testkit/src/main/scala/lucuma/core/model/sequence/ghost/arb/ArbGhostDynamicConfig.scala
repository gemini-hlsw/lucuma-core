// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import lucuma.core.util.arb.ArbNewType
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen


trait ArbGhostDynamicConfig:

  import ArbGhostDetector.given
  import ArbNewType.given

  given Arbitrary[Ifu1FiberAgitator] =
    Arbitrary:
      arbitrary[Boolean].map(Ifu1FiberAgitator.apply)

  given Cogen[Ifu1FiberAgitator] =
    Cogen[Boolean].contramap(_.value)

  given Arbitrary[Ifu2FiberAgitator] =
    Arbitrary:
      arbitrary[Boolean].map(Ifu2FiberAgitator.apply)

  given Cogen[Ifu2FiberAgitator] =
    Cogen[Boolean].contramap(_.value)

  given Arbitrary[GhostDynamicConfig] =
    Arbitrary:
      for
        r  <- arbitrary[GhostDetector.Red]
        b  <- arbitrary[GhostDetector.Blue]
        a1 <- arbitrary[Ifu1FiberAgitator]
        a2 <- arbitrary[Ifu2FiberAgitator]
      yield GhostDynamicConfig(r, b, a1, a2)

  given Cogen[GhostDynamicConfig] =
    Cogen[(
      GhostDetector.Red,
      GhostDetector.Blue,
      Ifu1FiberAgitator,
      Ifu2FiberAgitator
    )].contramap: a =>
      (
        a.redCamera,
        a.blueCamera,
        a.ifu1FiberAgitator,
        a.ifu2FiberAgitator
      )

object ArbGhostDynamicConfig extends ArbGhostDynamicConfig