// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import lucuma.core.model.sequence.arb.ArbAtomDigest
import lucuma.core.model.sequence.arb.ArbCategorizedTime
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbUid
import monocle.law.discipline.*
import munit.*

final class AtomDigestSuite extends DisciplineSuite:

  import ArbAtomDigest.given
  import ArbCategorizedTime.given
  import ArbEnumerated.given
  import ArbUid.given

  checkAll("Eq[AtomDigest]",          EqTests[AtomDigest].eqv)
  checkAll("AtomDigest.id",           LensTests(AtomDigest.id))
  checkAll("AtomDigest.observeClass", LensTests(AtomDigest.observeClass))
  checkAll("AtomDigest.timeEstimate", LensTests(AtomDigest.timeEstimate))
  checkAll("AtomDigest.hasArc",       LensTests(AtomDigest.hasArc))
  checkAll("AtomDigest.hasFlat",      LensTests(AtomDigest.hasFlat))