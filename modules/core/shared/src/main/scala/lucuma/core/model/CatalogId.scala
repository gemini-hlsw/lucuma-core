// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.CatalogName

final case class CatalogId(catalog: CatalogName, id: NonEmptyString)

object CatalogId {
  implicit val orderCatalogId: Order[CatalogId] =
    Order.by(x => (x.catalog, x.id))

  def apply(catalog: CatalogName, id: String): Option[CatalogId] =
    refineV[NonEmpty](id).toOption.map(i => CatalogId(catalog, i))
}
