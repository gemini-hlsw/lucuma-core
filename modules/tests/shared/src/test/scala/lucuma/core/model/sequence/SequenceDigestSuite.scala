// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbPlannedTime
import lucuma.core.model.sequence.arb.ArbSequenceDigest
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.*
import munit.*

final class SequenceDigestSuite extends DisciplineSuite {

  import ArbEnumerated.*
  import ArbPlannedTime.given
  import ArbSequenceDigest.given

  checkAll("Eq[SequenceDigest]",          EqTests[SequenceDigest].eqv)
  checkAll("SequenceDigest.observeClass", LensTests(SequenceDigest.observeClass))
  checkAll("SequenceDigest.plannedTime",  LensTests(SequenceDigest.plannedTime))
  checkAll("SequenceDigest.atomCount",    LensTests(SequenceDigest.atomCount))

}
