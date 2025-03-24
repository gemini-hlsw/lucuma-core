// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.kernel.laws.discipline.EqTests
import lucuma.catalog.arb.all.given

class FieldsSuite extends munit.DisciplineSuite {
  checkAll("Eq[FieldId]", EqTests[FieldId].eqv)
}
