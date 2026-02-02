// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2.arb

import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbIgrins2DynamicConfig:

  given Arbitrary[Igrins2DynamicConfig] = Arbitrary(
    arbitrary[TimeSpan].map(Igrins2DynamicConfig(_))
  )

  given Cogen[Igrins2DynamicConfig] =
    Cogen[TimeSpan].contramap(_.exposure)

object ArbIgrins2DynamicConfig extends ArbIgrins2DynamicConfig

trait ArbIgrins2StaticConfig:
  given Arbitrary[Igrins2StaticConfig] =
    Arbitrary:
      for {
        svc <- arbitrary[Igrins2SVCImages]
        off <- arbitrary[Igrins2OffsetMode]
      } yield Igrins2StaticConfig(svc, off)

  given Cogen[Igrins2StaticConfig] =
    Cogen[(Igrins2SVCImages, Igrins2OffsetMode)]
      .contramap(c => (c.saveSVCImages, c.offsetMode))

object ArbIgrins2StaticConfig extends ArbIgrins2StaticConfig
