// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Offset
import lucuma.core.math.RightAscension
import lucuma.core.model.AirMass
import lucuma.core.model.Ephemeris
import lucuma.core.model.Extinction
import lucuma.core.syntax.all.*

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeParseException
import scala.util.matching.Regex

import HorizonsConstants.HorizonsDateFormat
import HorizonsClient.Search

object HorizonsParser:

  // Parse an instant in the form "2025-Sep-09" "19:41:56" 
  private def parseInstant(ymd: String, hms: String): Either[String, Instant] =
    try LocalDateTime.parse(s"$ymd $hms", HorizonsDateFormat).toInstant(ZoneOffset.UTC).asRight
    catch
      case _: DateTimeParseException => s"Invalid date/time: $ymd $hms".asLeft

  // Parse a value that may be "n.a."
  private def parseNA[A,B](s: String, label: String)(f: String => Option[A])(g: A => Option[B]): Either[String, Option[B]] =
    s match
      case "n.a." => Right(None)
      case s      => f(s).map(g).toRight(s"Invalid $label: $s")

  // Parse a double that may be "n.a."
  extension [A](s: String) 
    private def parseDoubleOrNA(label: String)(into: Double => Option[A]): Either[String, Option[A]] =
       parseNA(s, label)(_.parseDoubleOption)(into)
    private def parseBigDecimalOrNA(label: String)(into: BigDecimal => Option[A]): Either[String, Option[A]] =
      parseNA(s, label)(_.parseBigDecimalOption)(into)
      
  // We select these columns
  //   1. Astrometric RA & DEC
  //   3. Rates; RA & DEC
  //   8. Airmass and Visual Magnitude Extinction
  //   9. Visual magnitude & surface Brightness
  // Example Line
  //  2025-Sep-09 19:41:56 *   07 22 20.773253 +22 04 10.77648  13.56443  -1.19117   1.046  0.114    5.840   5.331
  def parseEntry(line: String): Either[String, Ephemeris.Horizons.Element] = {

    // Split the line into exactly 14 fields, dropping column 3 if it shows up (it's often empty).
    val fields: Either[String, List[String]] = 
      val raw = line.trim.split("\\s+").toList
      raw.length match
        case 15 => raw.patch(2, Nil, 1).asRight // delete element 2
        case 14 => raw.asRight
        case a  => 
          s"""|Unexpected number of columns (expected 14 or 15, got $a): 
              |${line}
              |${raw.mkString(",")}
              |""".stripMargin.asLeft

    // Now parse the fields. It's easy enough to do this with existing parsers rather than building
    // up somehing with cats-parse.
    fields.flatMap: 
      case List(sYMD, sHMS, sRaH, sRaM, sRaS, sDecD, sDecM, sDecS, sRaRate, sDecRate, sAirmass, sExtinction, sVMag, sSB) =>
        (
          parseInstant(sYMD, sHMS),
          RightAscension.fromStringHMS.getOption(s"$sRaH:$sRaM:$sRaS").toRight(s"Invalid RA: $sRaH:$sRaM:$sRaS"),
          Declination.fromStringSignedDMS.getOption(s"$sDecD:$sDecM:$sDecS").toRight(s"Invalid Dec: $sDecD:$sDecM:$sDecS"),
          sRaRate.parseBigDecimalOption.map(Angle.fromBigDecimalArcseconds) toRight(s"Invalid RA delta: $sRaRate"),
          sDecRate.parseBigDecimalOption.map(Angle.fromBigDecimalArcseconds).toRight(s"Invalid Dec delta: $sRaRate"),
          sAirmass.parseDoubleOrNA("airmass")(AirMass.from(_).toOption),
          sExtinction.parseBigDecimalOrNA("extinction")(Extinction.FromVegaMagnitude.getOption),
          sVMag.parseDoubleOrNA("visual magnitude")(_.some),
          sSB.parseDoubleOrNA("surface brightness")(_.some)
        ).tupled.map: (inst, ra, dec, dRa, dDec, airmass, extinction, vmag, sb) =>
          Ephemeris.Horizons.Element(
            inst,
            Coordinates(ra, dec),
            Offset(
              Offset.P(dRa),
              Offset.Q(dDec)
            ),
            airmass,
            extinction,
            vmag,
            sb
          )
      case _ => sys.error("Logic error in HorizonsParser; expected exactly 14 columns.")

    }

  // N.B. The rest of this was lifted from HS2 in OCS, with no substantive changes.

  private type Offsets = List[(Int, Int)]
  private case class Row[A](a: A, name: String)

  // Parse many results based on an expected header pattern. This will read through the tail until
  // headers are found, then parse the remaining lines (until a blank line is encountered) using
  // the function constructed by `f`, if any (otherwise it is assumed that there were not enough
  // columns found). Returns a list of values or an error message.
  private def parseMany[A](header: String, tail: List[String], headerPattern: Regex)(f: Offsets => Option[String => A] ): Either[String, List[A]] =
    (headerPattern.findFirstMatchIn(header).toRight("Header pattern not found: " + headerPattern)) *>
    (tail.dropWhile(s => !s.trim.startsWith("---")) match {
      case Nil                => Nil.asRight // no column headers means no results!
      case colHeaders :: rows =>           // we have a header row with data rows following
        val offsets = "-+".r.findAllMatchIn(colHeaders).map(m => (m.start, m.end)).toList
        try
          f(offsets).map(g => rows.takeWhile(_.trim.nonEmpty).map(g)).toRight("Not enough columns.")
        catch
          case    nfe: NumberFormatException           =>  ("Number format exception: " + nfe.getMessage).asLeft
          case sioobe: StringIndexOutOfBoundsException =>   "Column value(s) not found.".asLeft
    })

  // Split into header/tail, and then parse. We don't know how many metadata lines precede the
  // start-of-data marker (which is a line with a bunch of asterisks on it) so we search for the
  // asterisks and drop all of the top material. The header is the following line and the tail is
  // the remainder.
  private def parseHeader[A](lines: List[String])(f: (String, List[String]) => Either[String, List[A]]): Either[String, List[A]] =
    lines.span(s => !s.contains("**********"))._2 match
      case _ :: h :: t => f(h, t)
      case other => "Could not find delimiter (asterisk) line.".asLeft

  def parseResponse[A](s: Search[A], lines: List[String]): Either[String, List[(A, String)]] =
    parseHeader[Row[A]](lines) { case (header, tail) =>
      s match

        case Search.Comet(_) =>

          // Common case is that we have many results, or none.
          lazy val case0 =
            parseMany[Row[Ephemeris.Key.Comet]](header, tail, """  +Small-body Index Search Results  """.r): os =>
              (os.lift(2), os.lift(3)).tupled.map:
                case ((ods, ode), (ons, one)) => row =>
                  val desig = row.substring(ods, ode).trim
                  val name  = row.substring(ons     ).trim // last column, so no end index because rows are ragged
                  Row(Ephemeris.Key.Comet(desig), name)

          // Single result with form: JPL/HORIZONS      Hubble (C/1937 P1)     2015-Dec-31 11:40:21
          lazy val case1 =
            """  +([^(]+)\s+\((.+?)\)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.Comet(m.group(2)), m.group(1)))
              .toRight( "Could not match 'Hubble (C/1937 P1)' header pattern.")

          // Single result with form: JPL/HORIZONS         1P/Halley           2015-Dec-31 11:40:21
          lazy val case2 =
            """  +([^/]+)/(.+?)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.Comet(m.group(1)), m.group(2)))
              .toRight( "Could not match '1P/Halley' header pattern.")

          // First one that works!
          case0 orElse
          case1 orElse
          case2 orElse "Could not parse the header line as a comet".asLeft

        case Search.Asteroid(_) =>

          // Common case is that we have many results, or none.
          lazy val case0 =
            parseMany[Row[Ephemeris.Key.Asteroid]](header, tail, """  +Small-body Index Search Results  """.r): os =>
              (os.lift(0), os.lift(1), os.lift(2)).tupled.map:
                case ((ors, ore), (ods, ode), (ons, one)) => row =>
                  val rec   = row.substring(ors, ore).trim.toInt
                  val desig = row.substring(ods, ode).trim
                  val name  = row.substring(ons     ).trim // last column, so no end index because rows are ragged
                  desig match
                    case "(undefined)" => Row(Ephemeris.Key.AsteroidOld(rec): Ephemeris.Key.Asteroid, name)
                    case des           => Row(Ephemeris.Key.AsteroidNew(des): Ephemeris.Key.Asteroid, name)

          // Single result with form: JPL/HORIZONS      90377 Sedna (2003 VB12)     2015-Dec-31 11:40:21
          lazy val case1 =
            """  +\d+ ([^(]+)\s+\((.+?)\)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.AsteroidNew(m.group(2)) : Ephemeris.Key.Asteroid, m.group(1)))
              .toRight("Could not match '90377 Sedna (2003 VB12)' header pattern.")

          // Single result with form: JPL/HORIZONS      4 Vesta     2015-Dec-31 11:40:21
          lazy val case2 =
            """  +(\d+) ([^(]+?)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.AsteroidOld(m.group(1).toInt) : Ephemeris.Key.Asteroid, m.group(2)))
              .toRight("Could not match '4 Vesta' header pattern.")

          // Single result with form: JPL/HORIZONS    (2016 GB222)    2016-Apr-20 15:22:36
          lazy val case3 =
            """  +\((.+?)\)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.AsteroidNew(m.group(1)) : Ephemeris.Key.Asteroid, m.group(1)))
              .toRight("Could not match '(2016 GB222)' header pattern.")

          // Single result with form: JPL/HORIZONS        418993 (2009 MS9)            2016-Sep-07 18:23:54
          lazy val case4 =
            """  +\d+\s+\((.+?)\)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.AsteroidNew(m.group(1)) : Ephemeris.Key.Asteroid, m.group(1)))
              .toRight("Could not match '418993 (2009 MS9)' header pattern.")

          // Single result with form: JPL/HORIZONS              1I/'Oumuamua (A/2017 U1)         2018-Apr-16 18:28:59
          lazy val case5 =
            """  +\S+\s+\((A/.+?)\)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.AsteroidNew(m.group(1)) : Ephemeris.Key.Asteroid, m.group(1)))
              .toRight("Could not match '1I/'Oumuamua (A/2017 U1)' header pattern.")

          // Single result with form: JPL/HORIZONS     A/2017 U7     2015-Dec-31 11:40:21
          lazy val case6 =
            """  +(A/\d+ [^(]+?)  """.r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.AsteroidNew(m.group(1)) : Ephemeris.Key.Asteroid, m.group(1)))
              .toRight("Could not match 'A/2017 U7' header pattern.")

          // First one that works!
          case0 orElse
          case1 orElse
          case2 orElse
          case3 orElse
          case4 orElse
          case5 orElse
          case6 orElse  "Could not parse the header line as an asteroid".asLeft

        case Search.MajorBody(_) =>

          // Common case is that we have many results, or none.
          lazy val case0 =
            this
              .parseMany[Row[Ephemeris.Key.MajorBody]](header, tail, """Multiple major-bodies match string""".r): os =>
                (os.lift(0), os.lift(1)).tupled.map:
                  case ((ors, ore), (ons, one)) => row =>
                    val rec  = row.substring(ors, ore).trim.toInt
                    val name = row.substring(ons, one).trim
                    Row(Ephemeris.Key.MajorBody(rec.toInt), name)
              .map(_.filterNot(_.a.num < 0)) // filter out spacecraft

          // Single result with form:  Revised: Aug 11, 2015       Charon / (Pluto)     901
          lazy val case1 =
            """  +(.*?) / \((.+?)\)  +(\d+) *$""".r
              .findFirstMatchIn(header)
              .map: m =>
                List(Row(Ephemeris.Key.MajorBody(m.group(3).toInt), m.group(1)))
              .toRight("Could not match 'Charon / (Pluto)     901' header pattern.")

          // First one that works, otherwise Nil because it falls through to small-body search
          case0 orElse
          case1 orElse Nil.asRight

    }.map(_.map(r => (r.a, r.name)))
