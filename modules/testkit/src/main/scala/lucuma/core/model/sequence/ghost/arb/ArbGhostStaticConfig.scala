// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import lucuma.core.enums.GhostResolutionMode
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbGhostStaticConfig:

  import ArbEnumerated.given
  import ArbGhostIfuMapping.given
  import ArbTimeSpan.given

  given Arbitrary[GhostStaticConfig] =
    Arbitrary:
      for
        r <- arbitrary[GhostResolutionMode]
        m <- arbitrary[Option[GhostIfuMapping]]
        s <- arbitrary[Option[TimeSpan]]
      yield GhostStaticConfig(r, m, s)

  given Cogen[GhostStaticConfig] =
    Cogen[(
      GhostResolutionMode,
      Option[GhostIfuMapping],
      Option[TimeSpan]
    )].contramap: a =>
      (
        a.resolutionMode,
        a.ifuMapping,
        a.slitViewingCameraExposureTime
      )

object ArbGhostStaticConfig extends ArbGhostStaticConfig