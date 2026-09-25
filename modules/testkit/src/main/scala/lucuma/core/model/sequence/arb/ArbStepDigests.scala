// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbStepDigests:
  import ArbStepDigest.given

  given Arbitrary[StepDigests] =
    Arbitrary:
      for
        b <- arbitrary[StepDigest]
        d <- arbitrary[StepDigest]
        r <- arbitrary[StepDigest]
        f <- arbitrary[StepDigest]
        o <- arbitrary[StepDigest]
      yield StepDigests(b, d, r, f, o)

  given Cogen[StepDigests] =
    Cogen[(StepDigest, StepDigest, StepDigest, StepDigest, StepDigest)]
      .contramap(a => (a.biases, a.darks, a.arcs, a.flats, a.observing))

object ArbStepDigests extends ArbStepDigests
