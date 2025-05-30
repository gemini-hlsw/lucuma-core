// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angular
import lucuma.core.math.Arc
import lucuma.core.math.Declination
import lucuma.core.math.Region
import lucuma.core.math.RightAscension
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*

trait ArbRegion:
  import ArbDeclination.given
  import ArbRightAscension.given
  import ArbArc.given

  given Arbitrary[Region] =
    Arbitrary(arbitrary[(Arc[RightAscension], Arc[Declination])].map(Region.apply))

  given Cogen[Region] =
    Cogen[(Arc[RightAscension], Arc[Declination])].contramap(a => (a.raArc, a.decArc))

object ArbRegion extends ArbRegion
