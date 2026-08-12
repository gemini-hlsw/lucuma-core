// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.syntax.all.*

/** A decoded value from one field of a binary table row. */
enum FitsCell:
  case IntCell(value: Long)
  case RealCell(value: Double)
  case TextCell(value: String)

  def asLong: Option[Long] = this match
    case IntCell(v)  => v.some
    case RealCell(v) => v.toLong.some
    case TextCell(_) => none

  def asInt: Option[Int] = asLong.map(_.toInt)

  def asDouble: Option[Double] = this match
    case IntCell(v)  => v.toDouble.some
    case RealCell(v) => v.some
    case TextCell(_) => none

  def asString: Option[String] = this match
    case TextCell(v) => v.some
    case _           => none

  /** The first character of a text cell, for the single character fields FITS tables use. */
  def asChar: Option[Char] = asString
    .map(_.trim)
    .collect:
      case s if s.nonEmpty => s.charAt(0)

/**
 * The subset of TFORM codes this reader supports.
 */
enum FitsColumnFormat(val code: Char, val unitWidth: Int):
  case Int32   extends FitsColumnFormat('J', 4)
  case Float32 extends FitsColumnFormat('E', 4)
  case Float64 extends FitsColumnFormat('D', 8)
  case Text    extends FitsColumnFormat('A', 1)

object FitsColumnFormat:
  def fromCode(c: Char): Option[FitsColumnFormat] =
    values.find(_.code === c)

/**
 * One column of a binary table.
 *
 * @param index
 *   one based position of the column in the table
 * @param name
 *   value of the column's TTYPE keyword
 * @param format
 *   element type
 * @param repeat
 *   element count; only text columns may exceed one
 * @param unit
 *   value of the column's TUNIT keyword, if present
 * @param byteOffset
 *   offset of this column within a row
 */
case class FitsColumn(
  index:      Int,
  name:       String,
  format:     FitsColumnFormat,
  repeat:     Int,
  unit:       Option[String],
  byteOffset: Int
):
  def byteWidth: Int = format.unitWidth * repeat

/**
 * The structure of a binary table extension: everything needed to decode its rows.
 *
 * Callers should resolve column names to indices '''once''' via [[indexOf]] and then read rows
 * positionally. Building a keyed map per row is the avoidable cost in a fixed stride format.
 */
case class FitsBinaryTable(
  header:    FitsHeader,
  columns:   Vector[FitsColumn],
  rowLength: Int,
  rowCount:  Long
):

  private lazy val indices: Map[String, Int] =
    columns.zipWithIndex.foldLeft(Map.empty[String, Int]):
      case (m, (c, i)) => if m.contains(c.name) then m else m.updated(c.name, i)

  /** Position of the named column, first occurrence winning. */
  def indexOf(name: String): Option[Int] =
    indices.get(name)

  def column(name: String): Option[FitsColumn] =
    indexOf(name).map(columns)

  /** Names present in this table, in file order. */
  def columnNames: Vector[String] =
    columns.map(_.name)

object FitsBinaryTable:

  /** Parses `TFORM` as an optional repeat count followed by a type code. */
  private def parseFormat(
    name:  String,
    tform: String
  ): Either[FitsProblem, (FitsColumnFormat, Int)] =
    val t        = tform.trim
    val digits   = t.takeWhile(_.isDigit)
    val codePart = t.drop(digits.length)
    val repeat   = if digits.isEmpty then 1 else digits.toIntOption.getOrElse(0)
    val problem  = FitsProblem.UnsupportedColumnFormat(name, tform)
    for
      code <- codePart.headOption.toRight(problem)
      fmt  <- FitsColumnFormat.fromCode(code).toRight(problem)
      // Only text columns may repeat: a numeric vector column would be an array field, which is
      // outside the scope declared on FitsColumnFormat.
      _    <- Either.cond(repeat > 0 && (repeat === 1 || fmt == FitsColumnFormat.Text), (), problem)
    yield (fmt, repeat)

  /** Builds a table description from a binary table extension header. */
  def fromHeader(header: FitsHeader): Either[FitsProblem, FitsBinaryTable] =
    for
      rowLength <- header.requireInt("NAXIS1")
      rowCount  <- header.requireInt("NAXIS2")
      fields    <- header.requireInt("TFIELDS")
      columns   <- (1 to fields).toVector
                     .foldLeft((Vector.empty[FitsColumn], 0).asRight[FitsProblem]):
                       case (acc, i) =>
                         acc.flatMap: (cols, offset) =>
                           val name  = header.string(s"TTYPE$i").map(_.trim).getOrElse(s"COL$i")
                           val tform = header.string(s"TFORM$i").getOrElse("")
                           parseFormat(name, tform).map: (fmt, repeat) =>
                             val col = FitsColumn(
                               i,
                               name,
                               fmt,
                               repeat,
                               header.string(s"TUNIT$i").map(_.trim).filter(_.nonEmpty),
                               offset
                             )
                             (cols :+ col, offset + col.byteWidth)
                     .map(_._1)
      computed   = columns.map(_.byteWidth).sum
      _         <- Either.cond(
                     computed === rowLength,
                     (),
                     FitsProblem.RowLengthMismatch(rowLength, computed)
                   )
    yield FitsBinaryTable(header, columns, rowLength, rowCount.toLong)
