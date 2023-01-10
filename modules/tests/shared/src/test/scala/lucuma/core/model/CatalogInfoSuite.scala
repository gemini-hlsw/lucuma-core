// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.string._
import lucuma.core.arb._
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb._
import monocle.law.discipline.LensTests
import munit._

final class CatalogInfoSuite extends DisciplineSuite {
  import ArbEnumerated._
  import ArbGid._
  import ArbCatalogInfo._

  // Laws
  checkAll("Order[CatalogInfo]", OrderTests[CatalogInfo].order)

  // Optics
  checkAll("CatalogInfo.catalog", LensTests(CatalogInfo.catalog))
  checkAll("CatalogInfo.id", LensTests(CatalogInfo.id))
  checkAll("CatalogInfo.objectType", LensTests(CatalogInfo.objectType))
}
