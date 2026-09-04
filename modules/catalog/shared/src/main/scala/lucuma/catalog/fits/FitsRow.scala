// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.syntax.all.*

/** One decoded row of a binary table, together with the structure needed to interpret it. */
case class FitsRow(table: FitsBinaryTable, cells: IndexedSeq[FitsCell]):

  /** The cell at a zero based column index. */
  def apply(index: Int): Option[FitsCell] =
    if index >= 0 && index < cells.length then cells(index).some else None

  /** The cell of the named column. Prefer `table.indexOf(name)` once, then [[apply]]. */
  def get(name: String): Option[FitsCell] =
    table.indexOf(name).flatMap(apply)
