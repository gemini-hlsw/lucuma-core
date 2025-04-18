// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.LensTests
import munit.*

final class CatalogInfoSuite extends DisciplineSuite {
  import ArbCatalogInfo.given

  // Laws
  checkAll("Order[CatalogInfo]", OrderTests[CatalogInfo].order)

  // Optics
  checkAll("CatalogInfo.catalog", LensTests(CatalogInfo.catalog))
  checkAll("CatalogInfo.id", LensTests(CatalogInfo.id))
  checkAll("CatalogInfo.objectType", LensTests(CatalogInfo.objectType))
}
