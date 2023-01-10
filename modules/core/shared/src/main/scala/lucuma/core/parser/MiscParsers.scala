// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.parser

import cats.parse.Numbers.digit
import cats.parse.Numbers.nonZeroDigit
import cats.parse.Parser.*
import cats.parse.Rfc5234.sp
import cats.parse.*
import lucuma.core.math.Index
import lucuma.core.optics.syntax.prism.*

import scala.annotation.tailrec

/** General-purpose parsers and combinators that aren't provided by cats-parse. */
trait MiscParsers {

  /** Creates a Parser that consumes the given character. */
  private def matchChar(c: Char, n: String): Parser[Unit] =
    char(c).void.withContext(n)

  /** Parser for a colon. */
  val colon: Parser[Unit] =
    matchChar(':', "colon")

  val colonOrSpace: Parser[Unit] = colon | sp

  /** Catch a `NumberFormatException`, useful for flatMap. */
  def catchNFE[A, B](f: A => B): A => Option[B] =
    a =>
      try Some(f(a))
      catch { case e: NumberFormatException => None }

  /** Parser for `n` consecutive digits, parsed as an `Int`. */
  def intN(n: Int): Parser[Int]       =
    digit.repExactlyAs[List[Char]](n).mapFilter(catchNFE(_.mkString.toInt)).withContext(s"intN($n)")

  /** Parser for an `Index`, which must be a positive Short with no leading zeros. */
  val index: Parser[Index] =
    (nonZeroDigit ~ digit.rep.?).mapFilter{(head, tail) =>
      val s: Int = tail.map(tail => (head :: tail).toList.mkString).getOrElse(head.toString).toInt
      catchNFE((i: Int) => Index.fromShort.getOption(i.toShort))(s).filter(_ => s.isValidInt).flatten
    }

  /** Parser for an optional sign (+ or -), returned as a boolean indicating whether to negate. */
  val neg: Parser0[Boolean]      =
    char('-').map(_ => true).backtrack | char('+').map(_ => false).backtrack | Parser.pure(false)
      .withContext("neg")

}

object MiscParsers extends MiscParsers
