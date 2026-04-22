// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs.arb

import lucuma.core.enums.GnirsWellDepth
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbGnirsStaticConfig:
  given Arbitrary[GnirsStaticConfig] = Arbitrary:
    for wellDepth <- arbitrary[GnirsWellDepth]
    yield GnirsStaticConfig(wellDepth)

  given Cogen[GnirsStaticConfig] =
    Cogen[GnirsWellDepth].contramap(_.wellDepth)

object ArbGnirsStaticConfig extends ArbGnirsStaticConfig
