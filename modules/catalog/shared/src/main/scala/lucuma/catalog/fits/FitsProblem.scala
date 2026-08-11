// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

/** Indicates a structural problem reading a FITS file. */
sealed trait FitsProblem extends Throwable with Product with Serializable:
  def displayValue: String

  override def getMessage: String = displayValue

  override def toString(): String = displayValue

object FitsProblem:

  /** The stream does not begin with a FITS primary header. */
  case object NotFitsFormat extends FitsProblem:
    val displayValue = "Not a FITS file: the primary header does not begin with SIMPLE"

  /** A header ran to the end of the stream without an END card. */
  case object UnterminatedHeader extends FitsProblem:
    val displayValue = "Malformed FITS header: no END card before the end of the file"

  /** A block was shorter than the 2880 bytes the format requires. */
  case class IncompleteBlock(bytesRead: Long) extends FitsProblem:
    val displayValue = s"Truncated FITS file: incomplete 2880 byte block, got $bytesRead bytes"

  /** No binary table extension was found in the file. */
  case object NoBinaryTable extends FitsProblem:
    val displayValue = "No BINTABLE extension found in the FITS file"

  /** A keyword required to interpret the file is absent. */
  case class MissingKeyword(keyword: String) extends FitsProblem:
    val displayValue = s"Missing required FITS keyword: $keyword"

  /** A keyword is present but its value could not be read as the expected type. */
  case class InvalidKeyword(keyword: String, value: String) extends FitsProblem:
    val displayValue = s"Invalid value for FITS keyword $keyword: '$value'"

  /** A column uses a TFORM this reader does not support. */
  case class UnsupportedColumnFormat(column: String, tform: String) extends FitsProblem:
    val displayValue =
      s"Unsupported TFORM '$tform' for column '$column'. Supported: nA, 1J, 1E, 1D"

  /**
   * The declared row length disagrees with the sum of the column widths.
   *
   * Worth failing on rather than trusting either number: a header that lies about the stride
   * decodes every subsequent row at the wrong offset, producing plausible but wrong values.
   */
  case class RowLengthMismatch(declared: Int, computed: Int) extends FitsProblem:
    val displayValue =
      s"NAXIS1 declares a row length of $declared bytes but the columns sum to $computed"

  /** The table ended before the declared number of rows had been read. */
  case class TruncatedData(expectedRows: Long, actualRows: Long) extends FitsProblem:
    val displayValue =
      s"Truncated FITS table: expected $expectedRows rows, found $actualRows"
