// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import cats.syntax.all.*
import lucuma.core.arb.ArbTime
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbExecutionConfig {
  import ArbAtom.given
  import ArbEnumerated.given
  import ArbSequence.given
  import ArbSetupTime.given
  import ArbTimeSpan.given

  given [S: Arbitrary, D: Arbitrary]: Arbitrary[ExecutionConfig[S, D]] =
    Arbitrary {
      for {
        s <- arbitrary[S]
        a <- arbitrary[Option[Sequence[D]]]
        n <- arbitrary[Option[Sequence[D]]]
        t <- arbitrary[SetupTime]
      } yield ExecutionConfig(s, a, n, t)
    }

  given [S: Cogen, D: Cogen]: Cogen[ExecutionConfig[S, D]] =
    Cogen[(S, Option[Sequence[D]], Option[Sequence[D]], SetupTime)].contramap { a =>
      (a.static, a.acquisition, a.science, a.setup)
    }

}

object ArbExecutionConfig extends ArbExecutionConfig
