// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.sequence.arb.ArbGcalDigest.given
import lucuma.core.util.arb.ArbTimeSpan.given
import monocle.law.discipline.*
import munit.*

class GcalDigestSuite extends DisciplineSuite:
  checkAll("Eq[GcalDigest]",     EqTests[GcalDigest].eqv)
  checkAll("Monoid[GcalDigest]", MonoidTests[GcalDigest].monoid)
  checkAll("GcalDigest.count",   LensTests(GcalDigest.count))
  checkAll("GcalDigest.time",    LensTests(GcalDigest.time))
