// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2.arb

import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.f2.F2StaticConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbF2StaticConfig:

  import ArbEnumerated.given

  given Arbitrary[F2StaticConfig] = Arbitrary(
    for
      mosPreImaging          <- arbitrary[MosPreImaging]
      useElectronicOffseting <- arbitrary[Boolean]
    yield F2StaticConfig(mosPreImaging, useElectronicOffseting)
  )

  given Cogen[F2StaticConfig] =
    Cogen[(MosPreImaging, Boolean)]
      .contramap(s => (s.mosPreImaging, s.useElectronicOffseting))

object ArbF2StaticConfig extends ArbF2StaticConfig
