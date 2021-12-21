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
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbCatalogInfo {
  import ArbEnumerated._

  implicit val arbCatalogInfo: Arbitrary[CatalogInfo] =
    Arbitrary {
      for {
        name    <- arbitrary[CatalogName]
        id      <- arbitrary[NonEmptyString]
        objType <- arbitrary[String]
      } yield CatalogInfo(name, id, objType)
    }

  implicit val cogSemester: Cogen[CatalogInfo] =
    Cogen[(CatalogName, String, String)].contramap(s => (s.catalog, s.id.value, s.objectType))

}

object ArbCatalogInfo extends ArbCatalogInfo
