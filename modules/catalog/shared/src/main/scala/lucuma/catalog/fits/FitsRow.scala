// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.syntax.all.*

/**
 * One decoded row of a binary table, together with the structure needed to interpret it.
 *
 * The reference to [[table]] is shared by every row of the table and costs a pointer. It is what
 * lets a consumer resolve column names to indices once, outside the row loop, and then read
 * positionally.
 */
case class FitsRow(table: FitsBinaryTable, cells: IndexedSeq[FitsCell]):

  /** The cell at a zero based column index. */
  def apply(index: Int): Option[FitsCell] =
    if index >= 0 && index < cells.length then cells(index).some else None

  /**
   * The cell of the named column.
   *
   * Resolving by name on every access defeats the purpose of the indexed representation. Prefer
   * `table.indexOf(name)` once, then [[apply]].
   */
  def get(name: String): Option[FitsCell] =
    table.indexOf(name).flatMap(apply)
