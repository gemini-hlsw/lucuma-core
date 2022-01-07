// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.CatalogName
import monocle.Focus
import monocle.Lens

final case class CatalogInfo(
  catalog:    CatalogName,
  id:         NonEmptyString,
  objectType: Option[NonEmptyString]
)

object CatalogInfo {
  implicit val orderCatalogInfo: Order[CatalogInfo] =
    Order.by(x => (x.catalog, x.id, x.objectType))

  def apply(
    catalog:    CatalogName,
    id:         String,
    objectType: String
  ): Option[CatalogInfo] =
    refineV[NonEmpty](id).toOption.map(i =>
      CatalogInfo(catalog, i, refineV[NonEmpty](objectType).toOption)
    )

  def apply(
    catalog: CatalogName,
    id:      String
  ): Option[CatalogInfo] =
    refineV[NonEmpty](id).toOption.map(i => CatalogInfo(catalog, i, none))

  /** @group Optics */
  val catalog: Lens[CatalogInfo, CatalogName] =
    Focus[CatalogInfo](_.catalog)

  /** @group Optics */
  val id: Lens[CatalogInfo, NonEmptyString] =
    Focus[CatalogInfo](_.id)

  /** @group Optics */
  val objectType: Lens[CatalogInfo, Option[NonEmptyString]] =
    Focus[CatalogInfo](_.objectType)

}
