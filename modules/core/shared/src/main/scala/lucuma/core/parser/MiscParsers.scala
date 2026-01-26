// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.parser

import cats.parse.*
import cats.parse.Numbers.digit
import cats.parse.Numbers.nonZeroDigit
import cats.parse.Parser.*
import cats.parse.Rfc5234.sp
import cats.parse.Rfc5234.wsp
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Index

import scala.util.control.Exception.allCatch
import scala.util.control.Exception.catching

/** General-purpose parsers and combinators that aren't provided by cats-parse. */
trait MiscParsers {

  /** Creates a Parser that consumes the given character. */
  private def matchChar(c: Char, n: String): Parser[Unit] =
    char(c).void.withContext(n)

  /** Parser for a colon. */
  val colon: Parser[Unit] =
    matchChar(':', "colon")

  val colonOrSpace: Parser[Unit] = colon | sp

  /** Parses a comma: ',' */
  val comma: Parser[Unit] =
    matchChar(',', "comma")

  /** Parses a dash: '-' */
  val dash: Parser[Unit] =
    matchChar('-', "dash")

  /** Catch a `NumberFormatException`, useful for mapFilter. */
  def catchNFE[A, B](f: A => B): A => Option[B] =
    a => catching(classOf[NumberFormatException]) opt f(a)

  /** Parses a signed integer. */
  val int: Parser[Int] =
    Numbers.signedIntString.mapFilter(catchNFE(_.toInt)).withContext("int")

  /** Parser for `n` consecutive digits, parsed as an `Int`. */
  def intN(n: Int): Parser[Int]       =
    digit.repExactlyAs[List[Char]](n).mapFilter(catchNFE(_.mkString.toInt)).withContext(s"intN($n)")

  /** Parser for an `Index`, which must be a positive Short with no leading zeros. */
  val index: Parser[Index] =
    (nonZeroDigit ~ digit.rep.?).mapFilter{(head, tail) =>
      val s: Int = tail.map(tail => (head :: tail).toList.mkString).getOrElse(head.toString).toInt
      catchNFE((i: Int) => Index.fromShort.getOption(i.toShort))(s).filter(_ => s.isValidInt).flatten
    }

  /** Parses optional whitespace (zero or more). */
  val maybeWhiteSpace: Parser0[Unit] =
    wsp.rep0.void

  /** Parses required whitespace (one or more). */
  val whitespace: Parser[Unit] =
    wsp.rep.void

  /** Parses a newline (LF or CRLF). */
  val newline: Parser[Unit] =
    char('\n').void | (char('\r') ~ char('\n').?).void

  /** Parses one or more non-whitespace characters. */
  val nonWhitespace: Parser[String] =
    charsWhile(c => !c.isWhitespace)

  /** Parses a blank line (newline or whitespace followed by newline/end). */
  val blankLine: Parser[Unit] =
    newline | (whitespace *> (newline | Parser.end))

  /** Parser for an optional sign (+ or -), returned as a boolean indicating whether to negate. */
  val neg: Parser0[Boolean]      =
    char('-').map(_ => true).backtrack | char('+').map(_ => false).backtrack | Parser.pure(false)
      .withContext("neg")

  /** Parses a PosBigDecimal. */
  val posBigDecimal: Parser0[PosBigDecimal] =
    (Numbers.digits0 ~ (Parser.char('.') ~ Numbers.digits).?)
      .string
      .mapFilter(s => allCatch.opt(BigDecimal(s)).flatMap(PosBigDecimal.unapply))
      .withContext("PosBigDecimal")

  /** Parses a PosInt. */
  val posInt: Parser[PosInt] =
    (Numbers.nonZeroDigit ~ Numbers.digits0)
      .string
      .mapFilter(s => allCatch.opt(s.toInt).flatMap(PosInt.unapply))
      .withContext("PosInt")
}

object MiscParsers extends MiscParsers
