// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2.arb

import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbIgrins2StaticConfig:

  given Arbitrary[Igrins2StaticConfig.type] =
    Arbitrary(Gen.const(Igrins2StaticConfig))

  given Cogen[Igrins2StaticConfig.type] =
    Cogen[Unit].contramap(_ => ())

object ArbIgrins2StaticConfig extends ArbIgrins2StaticConfig
