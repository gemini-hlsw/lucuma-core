// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.*
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CatalogName
import monocle.Focus
import monocle.Lens

import java.net.URI
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import scala.util.Try

final case class CatalogInfo(
  catalog:    CatalogName,
  id:         NonEmptyString,
  objectType: Option[NonEmptyString]
):
  val objectUrl: Option[URI] = catalog match
    case CatalogName.Simbad =>
      Try(URI(s"https://simbad.cds.unistra.fr/simbad/sim-id?Ident=${URLEncoder.encode(id.value, StandardCharsets.UTF_8.name())}")).toOption
    case _ => none


object CatalogInfo {
  given Order[CatalogInfo] = Order.by(x => (x.catalog, x.id, x.objectType))

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
