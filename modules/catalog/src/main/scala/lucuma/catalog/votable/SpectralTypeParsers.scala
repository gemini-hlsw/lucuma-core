// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Parser0
import cats.parse.Rfc5234.digit
import cats.syntax.eq.*

/**
 * Parser combinators for astronomical spectral type strings from Simbad.
 */
trait SpectralTypeParsers:

  /** Temperature class letter: O, B, A, F, G, K, M, L, T, Y */
  private val tempLetter: Parser[Char] =
    charIn("OBAFGKMLTY").withContext("temperature class letter")

  /** Temperature subclass: digit, optionally followed by decimal (e.g., "2", "3.5"), or empty */
  val tempSubclass: Parser0[String] =
    (digit ~ (char('.') ~ digit.rep).?).string.?.map(_.getOrElse("")).withContext("temperature subclass")

  /** Optional modifier: + or - */
  private val modifier: Parser0[String] =
    charIn("+-").string.?.map(_.getOrElse("")).withContext("modifier")

  /**
   * Temperature class: e.g., O9, G2, K3.5, M5+ Pattern: [OBAFGKMLTY][0-9]?(.5)?[+-]?
   */
  val tempClass: Parser[String] =
    (tempLetter ~ tempSubclass ~ modifier).string.withContext("temperature class")

  /** Luminosity class in Roman numerals */
  private val romanNumeral: Parser[String] =
    stringIn(List("VIII", "VII", "VI", "IV", "III", "II", "I", "V"))
      .withContext("Roman numeral")

  /** Optional luminosity subclass: a, b, or ab */
  private val lumSubclass: Parser0[String] =
    (string("ab").string | charIn("ab").string).?.map(_.getOrElse("")).withContext("luminosity subclass")

  /** Optional spectral peculiarity suffixes to skip: n (nebular), w (weak), p (peculiar), etc.
   *  When "n" suffix is present after luminosity class, it can affect temperature parsing
   *  (some systems treat O9.7n as O9n, ignoring fractional subclass)
   */
  private val peculiarSuffix: Parser0[Unit] =
    charIn("npwevhkmsq").rep0.void.withContext("peculiar suffix")

  /**
   * Luminosity class: e.g., V, III, Ia, IVb Pattern: (I|II|III|IV|V|VI|VII|VIII)[ab]?[npwe...]?
   */
  val lumClass: Parser[String] =
    ((romanNumeral ~ lumSubclass).string <* peculiarSuffix).withContext("luminosity class")

  /** Range separator: - or / */
  private val separator: Parser[Char] =
    charIn("-/").withContext("separator")

  private val partialTempClass: Parser[String] =
    (digit ~ (char('.') ~ digit).?).string.withContext("partial temperature class")

  /** Temperature class without trailing modifier (for ranges where "-" acts as separator) */
  private val tempClassNoMod: Parser[String] =
    (tempLetter ~ tempSubclass).string.withContext("temperature class no modifier")

  /**
   * Temperature range: e.g., G8, G8/K0, M8-9, M2/3, O9-9.5, F1-F2 Returns list of normalized temperature classes
   * Handles ambiguity where "-" can be modifier (K3-) or separator (F1-F2, O9-9.5)
   */
  val tempRange: Parser[List[String]] =
    (tempClass ~ (separator ~ (tempClass | partialTempClass)).rep0 ~ (tempClassNoMod | partialTempClass).?)
      .map { case ((first, rest), trailing) =>
        // If first ends with "-" or "+" and there's a trailing class,
        // treat the modifier as a separator
        val (baseFirst, extraClasses) =
          if ((first.endsWith("-") || first.endsWith("+")) && trailing.isDefined) then
            (first.dropRight(1), trailing.toList)
          else
            (first, Nil)

        val classes = baseFirst :: rest.map(_._2) ::: extraClasses
        // Normalize: if second starts with digit, prepend first letter
        if (classes.length > 1) {
          val firstLetter = baseFirst.charAt(0)
          classes.map { tc =>
            if (tc.nonEmpty && tc.charAt(0).isDigit && !tc.contains(firstLetter)) {
              s"$firstLetter$tc"
            } else tc
          }
        } else classes
      }
      .withContext("temperature range")

  /** Subclass-only luminosity: just "a", "b", or "ab" (used after a full luminosity class) */
  private val lumSubclassOnly: Parser[String] =
    charIn("ab").rep.string.withContext("luminosity subclass only")

  /**
   * Luminosity range: e.g., V, IV/V, IIIb, Iab/b Returns normalized luminosity classes
   */
  val lumRange: Parser[List[String]] =
    (lumClass ~ (separator ~ (lumClass.backtrack | lumSubclassOnly)).rep0)
      .map:
        case (first, rest) =>
          val classes = first :: rest.map(_._2)
          // From python: if second is just a/b, prepend first Roman numeral
          if (classes.length > 1) {
            val roman = first.takeWhile(c => c === 'I' || c === 'V')
            classes.map: lc =>
              if (lc.matches("[ab]+") && roman.nonEmpty) {
                s"$roman$lc"
              } else lc
          } else classes
      .withContext("luminosity range")

  /**
   * White dwarf temperature: "3.5" and ".8" formats Pattern: [0-9]+\.?[0-9]* | \.[0-9]+
   */
  private val whiteDwarfTemp: Parser[String] =
    ((digit.rep ~ (char('.') ~ digit.rep).?).backtrack | (char('.') ~ digit.rep)).string
      .withContext("white dwarf temperature")

  /**
   * White dwarf spectral type: e.g., DA3.5, DBAP3, DC, DA.8 Pattern: D[ABCGKMOQPXZ]*<temperature>?
   * Temperature can include decimal point
   */
  private val whiteDwarf: Parser[(List[String], List[String])] =
    (char('D') ~ charIn("ABCGKMOQPXZ").rep0 ~ whiteDwarfTemp.?)
      .map:
        case ((_, letters), wdTemp) =>
          val lumClass  = "D" + letters.mkString
          val tempClass = wdTemp.filter(_.nonEmpty).toList
          (List(lumClass), tempClass)
      .withContext("white dwarf")

  private val lenientTempClass: Parser[String] =
    (charIn("OBAFGKMLTYABCNPXZ") ~ (digit | charIn("OBAFGKMLTYABCNPXZ.+-")).rep0).string
      .withContext("lenient temperature class")

  /**
   * Subdwarf spectral type: e.g., sdO2, sdG, sdB1, sdBN0 Pattern: sd<temperature class>?
   */
  private val subdwarf: Parser[(List[String], List[String])] =
    (string("sd") *> lenientTempClass.?)
      .map:
        case Some(temp) => (List("sd"), List(temp))
        case None       => (List("sd"), List.empty)
      .withContext("subdwarf")

  private val skipToLuminosity: Parser0[Unit] =
    (not(peek(lumClass)).with1 *> anyChar).rep0.void.withContext("skip to luminosity")

  /**
   * Main sequence spectral type: e.g., G2V, K3III, G8/K0III, A0mA1Va Pattern: <tempRange><arbitrary
   * content>?<lumRange>? Handles Am star notation by skipping intermediate content before
   * luminosity class
   */
  private val mainSequence: Parser[(List[String], List[String])] =
    (tempRange ~ (skipToLuminosity *> lumRange).?)
      .map:
        case (temps, Some(lums)) => (lums, temps)
        case (temps, _)          => (List.empty, temps)
      .withContext("main sequence")

  /**
   * spectral type parser. Tries white dwarf, subdwarf, then main sequence in order.
   */
  val spectralType: Parser[(List[String], List[String])] =
    (whiteDwarf.backtrack | subdwarf.backtrack | mainSequence)
      .withContext("spectral type")

object SpectralTypeParsers extends SpectralTypeParsers
