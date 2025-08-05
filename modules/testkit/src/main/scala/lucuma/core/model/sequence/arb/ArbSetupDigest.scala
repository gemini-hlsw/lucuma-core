// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.scalacheck.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbSetupDigest:

  import ArbSetupTime.given

  given Arbitrary[SetupDigest] =
    Arbitrary:
      for
        s <- arbitrary[SetupTime]
        a <- arbitrary[NonNegInt]
        r <- arbitrary[NonNegInt]
      yield SetupDigest(s, a, r)

  given Cogen[SetupDigest] =
    Cogen[(
      SetupTime,
      NonNegInt,
      NonNegInt
    )].contramap(a => (a.setupTime, a.acquisitionCount, a.reacquisitionCount))

object ArbSetupDigest extends ArbSetupDigest