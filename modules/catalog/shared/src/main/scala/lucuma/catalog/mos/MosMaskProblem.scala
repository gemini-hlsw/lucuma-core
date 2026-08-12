// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.mos

import cats.data.NonEmptyList
import lucuma.catalog.fits.FitsProblem

/** Indicates a problem interpreting a FITS file as a MOS mask design. */
sealed trait MosMaskProblem extends Throwable with Product with Serializable:
  def displayValue: String

  override def getMessage: String = displayValue

  override def toString(): String = displayValue

object MosMaskProblem:

  /** The file is not structurally a readable FITS binary table. */
  case class Fits(problem: FitsProblem) extends MosMaskProblem:
    val displayValue = problem.displayValue

  /**
   * One or more columns a mask design must have are absent.
   *
   * Reported together rather than one at a time: someone fixing a hand built table wants the whole
   * list, not to rediscover the next missing column on every attempt.
   */
  case class MissingColumns(columns: NonEmptyList[String]) extends MosMaskProblem:
    val displayValue =
      s"Missing required MOS mask columns: ${columns.toList.mkString(", ")}"

  /** A keyword the design cannot be interpreted without is absent. */
  case class MissingKeyword(keyword: String) extends MosMaskProblem:
    val displayValue = s"Missing required MOS mask keyword: $keyword"

  /** A keyword is present but carries a value this library does not recognise. */
  case class InvalidKeyword(keyword: String, value: String) extends MosMaskProblem:
    val displayValue = s"Unrecognised value for MOS mask keyword $keyword: '$value'"

  /**
   * A cell holds a value outside the range the model allows.
   *
   * Carries the row index because a bare complaint about an unknown priority is not actionable
   * against a table of two hundred slits.
   */
  case class InvalidCell(row: Long, column: String, value: String) extends MosMaskProblem:
    val displayValue =
      s"Invalid value in column '$column' at row $row: '$value'"

  /** A cell that must be present is absent or of the wrong type. */
  case class MissingCell(row: Long, column: String) extends MosMaskProblem:
    val displayValue = s"Missing or untyped value in column '$column' at row $row"
