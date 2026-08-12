// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.fits

import cats.syntax.all.*

import scala.util.control.NonFatal

/**
 * The value carrying records of one FITS header data unit.
 *
 * Cards are retained in file order. Real files do contain the same keyword more than once — mask
 * files written by GMMPS carry `FILE_OT` twice, with different values — so lookups resolve to the
 * '''first''' occurrence, and [[cards]] remains available when the later ones matter.
 *
 * '''Not everything in the header reaches here.''' A card is retained only if it has a value
 * indicator in the ninth column and a keyword of at most eight characters. That excludes:
 *
 *   - `COMMENT`, `HISTORY` and blank keyword padding, which carry no value. Real files do contain
 *     these; every Gemini file examined while writing this carries `COMMENT` cards.
 *   - `CONTINUE` cards of the long string convention. A value split across cards is therefore
 *     '''truncated to its first segment''' rather than reassembled.
 *   - `HIERARCH` keywords, which exceed eight characters.
 *
 * None of the three appears in the mask files this reader was written for. Supporting them is
 * bounded work, and the place to do it is here, but it has not been done.
 */
case class FitsHeader(cards: List[FitsHeader.Card]):

  private lazy val byKeyword: Map[String, FitsHeader.Card] =
    cards.foldLeft(Map.empty[String, FitsHeader.Card]): (m, c) =>
      if m.contains(c.keyword) then m else m.updated(c.keyword, c)

  /** The first card with this keyword, if any. */
  def card(keyword: String): Option[FitsHeader.Card] =
    byKeyword.get(keyword)

  /** The raw, unparsed value of the first card with this keyword. */
  def raw(keyword: String): Option[String] =
    card(keyword).map(_.value)

  /**
   * Every retained keyword and its raw value, first occurrence winning.
   *
   * Subject to the exclusions described on this class: commentary cards, `CONTINUE` continuations
   * and `HIERARCH` keywords are not present.
   */
  lazy val rawValues: Map[String, String] =
    byKeyword.view.mapValues(_.value).toMap

  def string(keyword: String): Option[String] =
    raw(keyword).map(FitsHeader.unquote)

  def int(keyword: String): Option[Int] =
    raw(keyword).flatMap(s =>
      try s.trim.toInt.some
      catch { case NonFatal(_) => None }
    )

  def long(keyword: String): Option[Long] =
    raw(keyword).flatMap(s =>
      try s.trim.toLong.some
      catch { case NonFatal(_) => None }
    )

  def double(keyword: String): Option[Double] =
    raw(keyword).flatMap: s =>
      // FITS permits Fortran style exponents, e.g. 1.234D+05
      val n = s.trim.replace('D', 'E').replace('d', 'E')
      try n.toDouble.some
      catch { case NonFatal(_) => None }

  def boolean(keyword: String): Option[Boolean] =
    raw(keyword)
      .map(_.trim)
      .collect:
        case "T" => true
        case "F" => false

  /** True if this header describes a binary table extension. */
  def isBinaryTable: Boolean =
    string("XTENSION").exists(_.trim === "BINTABLE")

  /** True if this header is a primary header. */
  def isPrimary: Boolean =
    card("SIMPLE").isDefined

  private[fits] def requireInt(keyword: String): Either[FitsProblem, Int] =
    raw(keyword) match
      case None    => FitsProblem.MissingKeyword(keyword).asLeft
      case Some(v) =>
        int(keyword).toRight(FitsProblem.InvalidKeyword(keyword, v))

  /**
   * Size in bytes of this unit's data section, before padding to a block boundary.
   *
   * Follows the standard formula, which covers images and tables alike: a unit with `NAXIS = 0`
   * carries no data.
   */
  private[fits] def dataSize: Long =
    val naxis = int("NAXIS").getOrElse(0)
    if naxis <= 0 then 0L
    else
      val axes   = (1 to naxis).toList.traverse(i => long(s"NAXIS$i"))
      val bitpix = int("BITPIX").getOrElse(8)
      val pcount = long("PCOUNT").getOrElse(0L)
      val gcount = long("GCOUNT").getOrElse(1L)
      axes.fold(0L)(as => (math.abs(bitpix) / 8L) * gcount * (pcount + as.product))

object FitsHeader:

  /** One 80 character header record. */
  case class Card(keyword: String, value: String, comment: Option[String])

  private val Quote = '\''

  /** Strips the surrounding quotes of a FITS string value and unescapes doubled quotes. */
  def unquote(s: String): String =
    val t = s.trim
    if t.length >= 2 && t.head === Quote && t.last === Quote then
      t.substring(1, t.length - 1).replace("''", "'").trim
    else t

  /**
   * Parses one 80 character record.
   *
   * Returns `None` for the `END` card, for commentary keywords, and for blank padding — none of
   * which carry a value.
   */
  def parseCard(record: String): Option[Card] =
    val keyword = record.take(8).trim
    if keyword.isEmpty || keyword === "END" then None
    else if record.length < 10 || record.charAt(8) =!= '=' then
      // COMMENT, HISTORY and other commentary keywords have no value indicator.
      None
    else
      val rest             = record.drop(9)
      val (value, comment) = splitValue(rest)
      Card(keyword, value.trim, comment.map(_.trim).filter(_.nonEmpty)).some

  /**
   * Splits a card's value from its comment.
   *
   * A `/` inside a quoted string is part of the value, not a comment delimiter — file paths in
   * `FILENAME` keywords make this a real case rather than a theoretical one.
   */
  private def splitValue(s: String): (String, Option[String]) =
    val trimmed = s.dropWhile(_ === ' ')
    if trimmed.startsWith(Quote.toString) then
      var i      = 1
      var closed = -1
      while i < trimmed.length && closed < 0 do
        if trimmed.charAt(i) === Quote then
          if i + 1 < trimmed.length && trimmed.charAt(i + 1) === Quote then i += 2
          else { closed = i; i += 1 }
        else i += 1
      if closed < 0 then (trimmed, None)
      else
        val value = trimmed.take(closed + 1)
        val after = trimmed.drop(closed + 1)
        (value, after.dropWhile(_ === ' ').stripPrefix("/").some.filter(_ => after.contains('/')))
    else
      s.indexOf('/') match
        case -1 => (s, None)
        case i  => (s.take(i), s.drop(i + 1).some)
