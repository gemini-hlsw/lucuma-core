// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.parser.MiscParsers.blankLine
import lucuma.core.parser.MiscParsers.comma
import lucuma.core.parser.MiscParsers.maybeWhiteSpace
import lucuma.core.parser.MiscParsers.newline
import lucuma.core.parser.MiscParsers.nonWhitespace
import lucuma.core.parser.MiscParsers.whitespace
import lucuma.core.util.Enumerated

private[simbad] trait SEDDataParsers:

  // Ignore commented lines
  val comment: Parser[Unit] =
    Parser.char('#') *> Parser.charsWhile0(_ != '\n').void <* (newline | Parser.end)

  val csvToken: Parser[String] = Parser.charsWhile(c => !c.isWhitespace && c != ',')

  val spectrumByFilename: Map[String, StellarLibrarySpectrum] =
    Enumerated[StellarLibrarySpectrum].all.map(s => s.sedSpectrum -> s).toMap

  def lookupSpectrum(filename: String): Option[StellarLibrarySpectrum] =
    val baseName = filename.stripSuffix(".nm")
    spectrumByFilename.get(baseName)

  // Parses a data row: luminosity_class temperature_class filename
  val starsFileDataRow: Parser[Option[(StellarLibrarySpectrum, (List[String], List[String]))]] =
    (nonWhitespace ~ (whitespace *> nonWhitespace) ~ (whitespace *> nonWhitespace) <* maybeWhiteSpace <* (newline | Parser.end))
      .map { case ((lc, tc), filename) =>
        lookupSpectrum(filename).map(spectrum => (spectrum, (List(lc), List(tc))))
      }

  // Parses the header line "lc tc filename"
  val starsFileHeader: Parser[Unit] =
    (Parser.char('l') ~ Parser.char('c') ~ whitespace ~ Parser.char('t') ~ Parser.char(
      'c'
    ) ~ whitespace ~ Parser.string(
      "filename"
    ) ~ maybeWhiteSpace ~ (newline | Parser.end)).void

  // Parses any line in stars file
  val starsFileLine: Parser[Option[(StellarLibrarySpectrum, (List[String], List[String]))]] =
    comment.backtrack.as(None) |
      blankLine.backtrack.as(None) |
      starsFileHeader.backtrack.as(None) |
      starsFileDataRow

  // Parses entire stars file
  val starsFile: Parser0[List[(StellarLibrarySpectrum, (List[String], List[String]))]] =
    starsFileLine.rep0.map(_.flatten)

  // Parses gravity CSV header line starting with "Sp,Sc,..."
  val csvHeader: Parser[Unit] =
    (Parser.string("Sp") ~ maybeWhiteSpace ~ comma ~ maybeWhiteSpace ~
      Parser.string("Sc") ~ Parser.charsWhile0(_ != '\n').void ~ (newline | Parser.end)).void

  val csvField: Parser0[Option[Double]] =
    (maybeWhiteSpace *> Numbers.jsonNumber.? <* maybeWhiteSpace).map(_.map(_.toDouble))

  // Parses a gravity CSV row: spectral code and log(g) values per luminosity class
  val gravityRow: Parser[(Double, Map[String, Double])] =
    for
      _   <- csvToken <* maybeWhiteSpace <* comma
      sc  <- csvField <* comma
      v   <- csvField <* comma
      iv  <- csvField <* comma
      iii <- csvField <* comma
      ii  <- csvField <* comma
      ib  <- csvField <* comma
      iab <- csvField <* comma
      ia  <- csvField <* comma
      sd  <- csvField <* maybeWhiteSpace <* (newline | Parser.end)
    yield
      val code   = sc.getOrElse(0.0)
      val values = List(
        "V"   -> v,
        "IV"  -> iv,
        "III" -> iii,
        "II"  -> ii,
        "Ib"  -> ib,
        "Iab" -> iab,
        "Ia"  -> ia,
        "sd"  -> sd
      ).collect:
        case (k, Some(v)) => k -> v
      .toMap
      (code, values)

  // Parses any line in gravity file: comment, blank, header, or data row
  val gravityFileLine: Parser[Option[(Double, Map[String, Double])]] =
    comment.backtrack.as(None) |
      blankLine.backtrack.as(None) |
      csvHeader.backtrack.as(None) |
      gravityRow.map(Some(_))

  // Parses entire gravity CSV into list of (spectral_code, luminosity_class -> log_g) tuples
  val gravityFile: Parser0[List[(Double, Map[String, Double])]] =
    gravityFileLine.rep0.map(_.flatten)

private[simbad] object SEDDataParsers extends SEDDataParsers:
  def parseGravityFile(content: String): Either[Throwable, GravityTableConfig] =
    gravityFile.parseAll(content) match
      case Right(rows) => Right(GravityTableConfig(rows))
      case Left(error) => Left(RuntimeException(s"Failed to parse gravity file: ${error.toString}"))

  def parseStarsFile(content: String): Either[Throwable, StellarLibraryConfig] =
    starsFile.parseAll(content) match
      case Right(entries) => Right(StellarLibraryConfig(entries))
      case Left(error)    => Left(RuntimeException(s"Failed to parse stars file: ${error.toString}"))
