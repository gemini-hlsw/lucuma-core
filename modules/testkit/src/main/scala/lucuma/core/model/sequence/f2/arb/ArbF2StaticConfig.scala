// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2.arb

import lucuma.core.enums.*
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbF2StaticConfig:
  import ArbEnumerated.given

  given Arbitrary[F2StaticConfig] = Arbitrary(
    for
      mosPreImaging          <- arbitrary[Boolean]
      useElectronicOffseting <- arbitrary[Boolean]
      customSlitWidth        <- arbitrary[Option[F2CustomSlitWidth]]
    yield F2StaticConfig(mosPreImaging, useElectronicOffseting, customSlitWidth)
  )

  given Cogen[F2StaticConfig] =
    Cogen[(Boolean, Boolean, Option[F2CustomSlitWidth])]
      .contramap(s => (s.mosPreImaging, s.useElectronicOffseting, s.customSlitWidth))

object ArbF2StaticConfig extends ArbF2StaticConfig
