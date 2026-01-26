// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import cats.parse.Numbers
import cats.parse.Parser as P
import cats.parse.Parser0 as P0
import cats.parse.Rfc5234.wsp
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.util.Enumerated

private[simbad] object SEDDataParsers:
  private val whitespace: P[Unit]  = wsp.rep.void
  private val optionalWs: P0[Unit] = wsp.rep0.void
  private val newline: P[Unit]     = P.char('\n').void | (P.char('\r') ~ P.char('\n').?).void

  private val comment: P[Unit]   = P.char('#') *> P.charsWhile0(_ != '\n').void <* (newline | P.end)
  private val blankLine: P[Unit] = newline | (wsp.rep.void *> (newline | P.end))

  private val nonWhitespace: P[String] = P.charsWhile(c => !c.isWhitespace)
  private val csvToken: P[String]      = P.charsWhile(c => !c.isWhitespace && c != ',')

  private val spectrumByFilename: Map[String, StellarLibrarySpectrum] =
    Enumerated[StellarLibrarySpectrum].all.map(s => s.sedSpectrum -> s).toMap

  private def lookupSpectrum(filename: String): Option[StellarLibrarySpectrum] =
    val baseName = filename.stripSuffix(".nm")
    spectrumByFilename.get(baseName)

  val starsFileDataRow: P[Option[(StellarLibrarySpectrum, (List[String], List[String]))]] =
    (nonWhitespace ~ (whitespace *> nonWhitespace) ~ (whitespace *> nonWhitespace) <* optionalWs <* (newline | P.end))
      .map { case ((lc, tc), filename) =>
        lookupSpectrum(filename).map(spectrum => (spectrum, (List(lc), List(tc))))
      }

  val starsFileHeader: P[Unit] =
    (P.char('l') ~ P.char('c') ~ whitespace ~ P.char('t') ~ P.char('c') ~ whitespace ~ P.string(
      "filename"
    ) ~ optionalWs ~ (newline | P.end)).void

  val starsFileLine: P[Option[(StellarLibrarySpectrum, (List[String], List[String]))]] =
    comment.backtrack.as(None) |
      blankLine.backtrack.as(None) |
      starsFileHeader.backtrack.as(None) |
      starsFileDataRow

  val starsFile: P0[List[(StellarLibrarySpectrum, (List[String], List[String]))]] =
    starsFileLine.rep0.map(_.flatten)

  def parseStarsFile(content: String): Either[String, StellarLibraryConfig] =
    starsFile.parseAll(content) match
      case Right(entries) => Right(StellarLibraryConfig(entries))
      case Left(error)    => Left(s"Failed to parse stars file: ${error.toString}")

  val csvHeader: P[Unit] =
    (P.string("Sp") ~ optionalWs ~ P.char(',') ~ optionalWs ~ P
      .string("Sc") ~ P.charsWhile0(_ != '\n').void ~ (newline | P.end)).void

  val csvField: P0[Option[Double]] =
    (optionalWs *> Numbers.jsonNumber.? <* optionalWs).map(_.map(_.toDouble))

  val gravityRow: P[(Double, Map[String, Double])] =
    for
      _   <- csvToken <* optionalWs <* P.char(',')
      sc  <- csvField <* P.char(',')
      v   <- csvField <* P.char(',')
      iv  <- csvField <* P.char(',')
      iii <- csvField <* P.char(',')
      ii  <- csvField <* P.char(',')
      ib  <- csvField <* P.char(',')
      iab <- csvField <* P.char(',')
      ia  <- csvField <* P.char(',')
      sd  <- csvField <* optionalWs <* (newline | P.end)
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
      ).collect { case (k, Some(v)) => k -> v }.toMap
      (code, values)

  val gravityFileLine: P[Option[(Double, Map[String, Double])]] =
    comment.backtrack.as(None) |
      blankLine.backtrack.as(None) |
      csvHeader.backtrack.as(None) |
      gravityRow.map(Some(_))

  val gravityFile: P0[List[(Double, Map[String, Double])]] =
    gravityFileLine.rep0.map(_.flatten)

  def parseGravityFile(content: String): Either[String, GravityTableConfig] =
    gravityFile.parseAll(content) match
      case Right(rows) => Right(GravityTableConfig(rows))
      case Left(error) => Left(s"Failed to parse gravity file: ${error.toString}")
