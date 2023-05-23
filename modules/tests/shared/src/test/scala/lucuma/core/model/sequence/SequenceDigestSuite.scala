// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.monoid.*
import lucuma.core.model.sequence.arb.ArbSequenceDigest
import munit.*

final class SequenceDigestSuite extends DisciplineSuite {

  import ArbSequenceDigest.given

  checkAll("Eq[SequenceDigest]",     EqTests[SequenceDigest].eqv)
  checkAll("Monoid[SequenceDigest]", MonoidTests[SequenceDigest].monoid)
}
