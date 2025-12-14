// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Parser0
import cats.parse.Rfc5234.digit

/**
 * Parser combinators for astronomical spectral type strings from Simbad. Handles main sequence
 * stars, giants, supergiants, white dwarfs, and subdwarfs.
 */
trait SpectralTypeParsers:

  /** Temperature class letter: O, B, A, F, G, K, M, L, T, Y */
  private val tempLetter: Parser[Char] =
    charIn("OBAFGKMLTY").withContext("temperature class letter")

  /** Optional temperature subclass: digit, optionally followed by decimal */
  private val tempSubclass: Parser0[String] =
    (digit ~ (char('.') ~ digit.rep).?).string

  /** Optional modifier: + or - */
  private val modifier: Parser0[String] =
    charIn("+-").string.?.map(_.getOrElse(""))

  /**
   * Temperature class: e.g., O9, G2, K3.5, M5+ Pattern: [OBAFGKMLTY][0-9]?(.5)?[+-]?
   */
  val tempClass: Parser[String] =
    (tempLetter ~ tempSubclass ~ modifier).string.withContext("temperature class")

  /** Luminosity class in Roman numerals */
  private val romanNumeral: Parser[String] =
    stringIn(List("VIII", "VII", "VI", "IV", "III", "II", "I", "V"))
      .withContext("Roman numeral")

  /** Optional luminosity subclass: a or b */
  private val lumSubclass: Parser0[String] =
    charIn("ab").string.?.map(_.getOrElse(""))

  /**
   * Luminosity class: e.g., V, III, Ia, IVb Pattern: (I|II|III|IV|V|VI|VII|VIII)[ab]?
   */
  val lumClass: Parser[String] =
    (romanNumeral ~ lumSubclass).string.withContext("luminosity class")

  /** Range separator: - or / */
  private val separator: Parser[Char] =
    charIn("-/").withContext("separator")

  /** Just a digit or digit with subclass, used in ranges like "M2/3" */
  private val partialTempClass: Parser[String] =
    (digit ~ (char('.') ~ digit).?).string

  /**
   * Temperature range: e.g., G8, G8/K0, M8-9, M2/3 Returns list of normalized temperature classes
   */
  val tempRange: Parser[List[String]] =
    (tempClass ~ (separator ~ (tempClass | partialTempClass)).rep0)
      .map { case (first, rest) =>
        val classes = first :: rest.map(_._2)
        // Normalize: if second starts with digit, prepend first letter
        if (classes.length > 1) {
          val firstLetter = first.charAt(0)
          classes.map { tc =>
            if (tc.charAt(0).isDigit && !tc.contains(firstLetter)) {
              s"$firstLetter$tc"
            } else tc
          }
        } else classes
      }
      .withContext("temperature range")

  /**
   * Luminosity range: e.g., V, IV/V, IIIb Returns list of normalized luminosity classes
   */
  val lumRange: Parser[List[String]] =
    (lumClass ~ (separator ~ lumClass).rep0)
      .map { case (first, rest) =>
        val classes = first :: rest.map(_._2)
        // Normalize: if second is just a/b, prepend first Roman numeral
        if (classes.length > 1) {
          val firstRoman = first.takeWhile(c => c == 'I' || c == 'V')
          classes.map { lc =>
            if (lc.matches("[ab]+") && firstRoman.nonEmpty) {
              s"$firstRoman$lc"
            } else lc
          }
        } else classes
      }
      .withContext("luminosity range")

  /**
   * White dwarf spectral type: e.g., DA3.5, DBAP3, DC Pattern: D[ABCGKMOQPXZ]*<temperature>?
   * Temperature can include decimal point
   */
  private val whiteDwarf: Parser[(List[String], List[String])] =
    (char('D') ~ charIn("ABCGKMOQPXZ").rep0 ~ (digit.rep ~ (char('.') ~ digit.rep).?).string.?)
      .map { case ((_, letters), tempOpt) =>
        val lumClass  = "D" + letters.mkString
        val tempClass = tempOpt.filter(_.nonEmpty).toList
        (List(lumClass), tempClass)
      }
      .withContext("white dwarf")

  /** Lenient temperature class for subdwarfs - just capture what we can */
  private val lenientTempClass: Parser[String] =
    (charIn("OBAFGKMLTYABCNPXZ") ~ (digit | charIn("OBAFGKMLTYABCNPXZ.+-")).rep0).string

  /**
   * Subdwarf spectral type: e.g., sdO2, sdG, sdB1, sdBN0 Pattern: sd<anything that looks like a
   * temperature class>?
   */
  private val subdwarf: Parser[(List[String], List[String])] =
    (string("sd") *> lenientTempClass.?)
      .map {
        case Some(temp) => (List("sd"), List(temp))
        case None       => (List("sd"), List.empty)
      }
      .withContext("subdwarf")

  /**
   * Main sequence spectral type: e.g., G2V, K3III, G8/K0III Pattern: <tempRange><lumRange>?
   */
  private val mainSequence: Parser[(List[String], List[String])] =
    (tempRange ~ lumRange.?)
      .map {
        case (temps, Some(lums)) => (lums, temps)
        case (temps, None)       => (List.empty, temps)
      }
      .withContext("main sequence")

  /**
   * Complete spectral type parser. Tries white dwarf, subdwarf, then main sequence in order.
   */
  val spectralType: Parser[(List[String], List[String])] =
    (whiteDwarf.backtrack | subdwarf.backtrack | mainSequence)
      .withContext("spectral type")

object SpectralTypeParsers extends SpectralTypeParsers
