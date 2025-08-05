// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbExecutionDigest:

  import ArbSequenceDigest.given
  import ArbSetupDigest.given

  given Arbitrary[ExecutionDigest] =
    Arbitrary:
      for
        c <- arbitrary[SetupDigest]
        a <- arbitrary[SequenceDigest]
        s <- arbitrary[SequenceDigest]
      yield ExecutionDigest(c, a, s)

  given Cogen[ExecutionDigest] =
    Cogen[(
      SetupDigest,
      SequenceDigest,
      SequenceDigest
    )].contramap(a => (a.setup, a.acquisition, a.science))

object ArbExecutionDigest extends ArbExecutionDigest