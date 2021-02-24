// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._
import lucuma.core.enum.CatalogName
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ArbCatalogId {
  import ArbEnumerated._

  implicit val arbCatalogId: Arbitrary[CatalogId] =
    Arbitrary {
      for {
        name <- arbitrary[CatalogName]
        id   <- arbitrary[NonEmptyString]
      } yield CatalogId(name, id)
    }

  implicit val cogSemester: Cogen[CatalogId] =
    Cogen[(CatalogName, String)].contramap(s => (s.catalog, s.id.value))

}

object ArbCatalogId extends ArbCatalogId
