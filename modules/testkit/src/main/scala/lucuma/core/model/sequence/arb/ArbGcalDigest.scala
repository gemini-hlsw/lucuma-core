// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbGcalDigest:
  import ArbTimeSpan.given

  given Arbitrary[GcalDigest] =
    Arbitrary:
      for
        c <- arbitrary[NonNegInt]
        t <- arbitrary[TimeSpan]
      yield GcalDigest(c, t)

  given Cogen[GcalDigest] =
    Cogen[(Int, TimeSpan)].contramap(a => (a.count.value, a.time))

object ArbGcalDigest extends ArbGcalDigest
