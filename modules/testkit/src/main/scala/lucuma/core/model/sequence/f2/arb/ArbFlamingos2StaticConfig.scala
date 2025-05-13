// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2.arb

import lucuma.core.enums.MosPreImaging
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbFlamingos2StaticConfig:

  import ArbEnumerated.given

  given Arbitrary[Flamingos2StaticConfig] = Arbitrary(
    for
      mosPreImaging          <- arbitrary[MosPreImaging]
      useElectronicOffseting <- arbitrary[Boolean]
    yield Flamingos2StaticConfig(mosPreImaging, useElectronicOffseting)
  )

  given Cogen[Flamingos2StaticConfig] =
    Cogen[(MosPreImaging, Boolean)]
      .contramap(s => (s.mosPreImaging, s.useElectronicOffseting))

object ArbFlamingos2StaticConfig extends ArbFlamingos2StaticConfig
