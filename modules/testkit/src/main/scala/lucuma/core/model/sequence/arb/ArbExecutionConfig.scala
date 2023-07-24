// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.model.sequence.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbExecutionConfig {
  import ArbExecutionSequence.given

  given [S: Arbitrary, D: Arbitrary]: Arbitrary[ExecutionConfig[S, D]] =
    Arbitrary {
      for {
        s <- arbitrary[S]
        a <- arbitrary[Option[ExecutionSequence[D]]]
        n <- arbitrary[Option[ExecutionSequence[D]]]
      } yield ExecutionConfig(s, a, n)
    }

  given [S: Cogen, D: Cogen]: Cogen[ExecutionConfig[S, D]] =
    Cogen[(S, Option[ExecutionSequence[D]], Option[ExecutionSequence[D]])].contramap { a =>
      (a.static, a.acquisition, a.science)
    }

}

object ArbExecutionConfig extends ArbExecutionConfig
