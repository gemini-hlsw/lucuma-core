// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.*
import cats.data.*
import cats.derived.*
import cats.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.*
import lucuma.catalog.votable.CatalogProblem.*

/** Describes a field */
case class FieldId(id: NonEmptyString, ucd: Option[Ucd]) derives Eq

object FieldId {
  def apply(id: String, ucd: Ucd): EitherNec[CatalogProblem, FieldId] =
    refineV[NonEmpty](id)
      .bimap(_ => NonEmptyChain.one(InvalidFieldId(id)).widen[CatalogProblem],
             FieldId(_, Some(ucd))
      )

  def unsafeFrom(id: String, ucd: Ucd): FieldId =
    apply(id, ucd).getOrElse(sys.error(s"Invalid field id $id"))

  def unsafeFrom(id: String): FieldId =
    refineV[NonEmpty](id)
      .bimap(_ => NonEmptyChain.one(InvalidFieldId(id)).widen[CatalogProblem], FieldId(_, None))
      .getOrElse(sys.error(s"Invalid field id $id"))

  def unsafeFrom(id: String, ucd: String): FieldId =
    Ucd(ucd).flatMap(apply(id, _)).getOrElse(sys.error(s"Invalid field id $id"))

}

case class TableRowItem(field: FieldId, data: String)

case class TableRow(items: List[TableRowItem]) {
  def itemsMap: Map[FieldId, String] = items.map(i => i.field -> i.data).toMap
}
