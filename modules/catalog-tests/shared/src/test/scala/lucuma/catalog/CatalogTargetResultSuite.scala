// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.kernel.laws.discipline.*
import lucuma.catalog.arb.all.given
import munit.*

class CatalogTargetResultSuite extends DisciplineSuite {

  // Laws
  checkAll("Eq[CatalogTargetResultSuite]", EqTests[CatalogTargetResult].eqv)
}
