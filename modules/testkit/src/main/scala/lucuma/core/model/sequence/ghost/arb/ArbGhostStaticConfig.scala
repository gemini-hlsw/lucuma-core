// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import lucuma.core.enums.GhostResolutionMode
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbGhostStaticConfig:

  import ArbEnumerated.given

  given Arbitrary[GhostStaticConfig] =
    Arbitrary:
      arbitrary[GhostResolutionMode].map(GhostStaticConfig.apply)

  given Cogen[GhostStaticConfig] =
    Cogen[GhostResolutionMode].contramap(_.resolutionMode)

object ArbGhostStaticConfig extends ArbGhostStaticConfig