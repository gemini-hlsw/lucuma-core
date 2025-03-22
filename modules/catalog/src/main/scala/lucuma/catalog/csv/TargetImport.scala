// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.csv

import cats.*
import cats.data.*
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import fs2.*
import fs2.data.csv.*
import lucuma.catalog.*
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import lucuma.catalog.votable.QueryByName
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.ProperMotion.AngularVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.RightAscension
import lucuma.core.math.dimensional.*
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.string.*
import lucuma.core.util.*
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client

import scala.collection.immutable.SortedMap

private case class TargetCsvRow(
  line:            Option[Long],
  name:            NonEmptyString,
  bare:            Boolean,
  ra:              Option[RightAscension],
  dec:             Option[Declination],
  pmRA:            Option[ProperMotion.RA],
  pmDec:           Option[ProperMotion.Dec],
  epoch:           Option[Epoch],
  brightnesses:    Map[Band, BrightnessValue],
  integratedUnits: Map[Band, Units Of Brightness[Integrated]],
  surfaceUnits:    Map[Band, Units Of Brightness[Surface]],
  parallax:        Option[Parallax],
  rv:              Option[RadialVelocity],
  z:               Option[Redshift]
) {

  private def brightnessAndUnit[A](
    brightnesses: Map[Band, BrightnessValue],
    units:        Map[Band, Units Of Brightness[A]],
    defaultUnit:  Band => Units Of Brightness[A]
  ): List[(Band, Measure[BrightnessValue] Of Brightness[A])] =
    brightnesses.map { b =>
      b._1 ->
        units
          .getOrElse(b._1, defaultUnit(b._1))
          .withValueTagged(b._2)
    }.toList

  private lazy val integratedBrightness
    : List[(Band, Measure[BrightnessValue] Of Brightness[Integrated])] =
    brightnessAndUnit(
      brightnesses,
      integratedUnits,
      _.defaultIntegrated.units
    )

  private lazy val surfaceBrightness
    : List[(Band, Measure[BrightnessValue] Of Brightness[Surface])] =
    brightnessAndUnit(
      brightnesses,
      surfaceUnits,
      _.defaultSurface.units
    )

  val sourceProfile: SourceProfile =
    if (surfaceUnits.nonEmpty)
      SourceProfile.Uniform(
        SpectralDefinition.BandNormalized(
          UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V).some,
          SortedMap(surfaceBrightness*)
        )
      )
    else
      SourceProfile.Point(
        SpectralDefinition.BandNormalized(
          None,
          SortedMap(integratedBrightness*)
        )
      )
}

object TargetImport extends ImportEpochParsers:
  class EmptyValue(msg: String, override val line: Option[Long] = None, inner: Throwable = null)
      extends DecoderError(msg, line, inner)

  extension [A](a: Option[A])
    def orError(r: String, prefix: String): DecoderResult[A] =
      a.toRight(
        if (r.isEmpty()) EmptyValue(s"Empty $prefix value")
        else DecoderError(s"Invalid $prefix value '$r'")
      )

  private given liftCellDecoder[A: CellDecoder]: CellDecoder[Option[A]] = s =>
    s.nonEmpty.guard[Option].traverse(_ => CellDecoder[A].apply(s))

  private given CellDecoder[NonEmptyString] =
    CellDecoder.stringDecoder.emap(r =>
      refineV[NonEmpty](r).leftMap(_ => new DecoderError("Empty name"))
    )

  private given ParseableHeader[String] =
    _.map(_.trim).asRight

  private given decDecoder: CellDecoder[Declination] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      Declination.fromStringSignedDMS
        .getOption(t)
        .orError(t, "Dec")
    }

  private given raDecoder: CellDecoder[RightAscension] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      RightAscension.lenientFromStringHMS
        .getOption(t)
        .orError(t, "RA")
    }

  private given CellDecoder[Epoch] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      epochParser
        .parseAll(t)
        .toOption
        .orError(t, "epoch")
    }

  private given CellDecoder[Parallax] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      t.parseBigDecimalOption
        .map(Parallax.milliarcseconds.reverseGet)
        .orError(t, "parallax")
    }

  private given CellDecoder[RadialVelocity] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      t.parseBigDecimalOption
        .flatMap(RadialVelocity.kilometerspersecond.getOption)
        .orError(t, "rv")
    }

  private given CellDecoder[Redshift] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      t.parseBigDecimalOption
        .map(Redshift.apply)
        .orError(t, "z")
    }

  private given CellDecoder[BrightnessValue] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      t.parseBigDecimalOption
        .flatMap(BrightnessValue.from(_).toOption)
        .orError(t, "z")
    }

  private def angularVelocityComponentDecoder[T](build: BigDecimal => T): CellDecoder[T] =
    CellDecoder.stringDecoder.emap { r =>
      val t = r.trim
      t.parseBigDecimalOption
        .map(r => build(r))
        .orError(t, "angular velocity")
    }

  private given pmRADecoder: CellDecoder[ProperMotion.RA] =
    angularVelocityComponentDecoder(ProperMotion.RA.milliarcsecondsPerYear.reverseGet)

  private given pmDecDecoder: CellDecoder[ProperMotion.Dec] =
    angularVelocityComponentDecoder(ProperMotion.Dec.milliarcsecondsPerYear.reverseGet)

  private def unitAbbv[A](using enumerated: Enumerated[Units Of Brightness[A]]) =
    enumerated.all.map(u => u.abbv -> u).toMap

  // Add some replacemente to ² and Å to support more variants of units
  private def expandedAbbrevations[T](using enumerated: Enumerated[Units Of Brightness[T]]) = {
    def replacedSquares(unit: (String, Units Of Brightness[T])) =
      if (unit._1.contains("²") || unit._1.contains("Å")) {
        Map(unit._1.replaceAll("²", "2").replaceAll("Å", "A")  -> unit._2,
            unit._1.replaceAll("²", "^2").replaceAll("Å", "A") -> unit._2
        )
      } else Map.empty

    unitAbbv[T].foldLeft(
      Map.empty[String, Units Of Brightness[T]]
    ) { (map, unit) =>
      val replaced  = replacedSquares(unit)
      val angstrom  = if (unit._1.contains("Å")) {
        Map(unit._1.replaceAll("Å", "A") -> unit._2, unit._1.replaceAll("Å", "A") -> unit._2)
      } else Map.empty
      val magSuffix = if (unit._1.contains(" mag")) {
        Map(unit._1.replaceAll(" mag", "") -> unit._2)
      } else Map.empty

      map ++ replaced ++ angstrom ++ angstrom.flatMap(replacedSquares) ++ magSuffix ++ magSuffix
        .flatMap(replacedSquares)
    }
  }

  // Add some well knwon synonyms
  private val integratedUnits: Map[String, Units Of Brightness[Integrated]] =
    unitAbbv[Integrated] ++ Map(
      "Vega"   -> VegaMagnitudeIsIntegratedBrightnessUnit.unit,
      "AB"     -> ABMagnitudeIsIntegratedBrightnessUnit.unit,
      "Jy"     -> JanskyIsIntegratedBrightnessUnit.unit,
      "Jansky" -> JanskyIsIntegratedBrightnessUnit.unit
    ) ++ expandedAbbrevations[Integrated]

  private val surfaceUnits: Map[String, Units Of Brightness[Surface]] =
    unitAbbv[Surface] ++ expandedAbbrevations[Surface]

  private given integratedDecoder: CellDecoder[Units Of Brightness[Integrated]] =
    CellDecoder.stringDecoder.emap(s =>
      integratedUnits.get(s.trim()).toRight(DecoderError(s"Unknown units $s"))
    )

  private given surfaceDecoder: CellDecoder[Units Of Brightness[Surface]] =
    CellDecoder.stringDecoder.emap(s =>
      surfaceUnits.get(s.trim()).toRight(DecoderError(s"Unknown units $s"))
    )

  private def brightnesses(row: CsvRow[String]): Map[Band, BrightnessValue] = Band.all
    .foldLeft(List.empty[(Band, Option[BrightnessValue])])((l, t) =>
      (t, row.as[Option[BrightnessValue]](t.shortName).toOption.flatten) :: l
    )
    .collect { case (b, Some(v)) =>
      (b, v)
    }
    .toMap

  private def units[T](using
    CellDecoder[Units Of Brightness[T]]
  )(row: CsvRow[String]): Map[Band, Units Of Brightness[T]] = Band.all
    .foldLeft(List.empty[(Band, Option[Units Of Brightness[T]])])((l, t) =>
      (t,
       row
         .as[Option[Units Of Brightness[T]]](s"${t.shortName}_unit")
         .toOption
         .flatten
      ) :: l
    )
    .collect { case (b, Some(v)) =>
      (b, v)
    }
    .toMap

  private def integratedUnits(row: CsvRow[String]) = units[Integrated](row)
  private def surfaceUnits(row:    CsvRow[String]) = units[Surface](row)

  extension [A](r: DecoderResult[Option[A]])
    // Check the result and populate the line where it failed
    def defaultToNone[B](row: CsvRow[B]): DecoderResult[Option[A]] = r match
      case a @ Right(_)                                          => a
      case Left(d) if d.getMessage().startsWith("unknown field") => Right(None)
      case Left(d)                                               =>
        d.withLine(row.line).asLeft

  extension (row: CsvRow[String])

    // try to find the value on any of the columns and fail if not found on any
    def asIn[A: CellDecoder](col: String, extras: String*): DecoderResult[A] =
      (col :: extras.toList)
        .collectFirst(row.as[A](_))
        .getOrElse(DecoderError(s"No column $col found").asLeft)

    private def internalAlternatives[T](
      alternatives: List[String],
      defaultValue: Option[T] = None
    )(using CellDecoder[Option[T]]): DecoderResult[Option[T]] = {
      val (errors, correct) = alternatives.toList
        .map(row.as[Option[T]](_))
        .partitionEither(identity)
      if (correct.nonEmpty) correct.head.asRight
      else if (errors.nonEmpty) {
        val fixableError = errors.forall { case d: DecoderError =>
          d.getMessage.startsWith("unknown field")
        } || errors.exists {
          case _: EmptyValue => true
          case _             => false
        }
        // if fixable go to default
        if (fixableError)
          defaultValue.asRight
        else
          errors.head.withLine(row.line).asLeft
      } else DecoderError(s"No column col found").withLine(row.line).asLeft
    }

    // Try to decode a column with several alternative capitalizations
    // If any fails the whole thing fails, if none is found return none
    def withAlternatives[T](
      col:          String,
      defaultValue: Option[T] = None
    )(using CellDecoder[Option[T]]): DecoderResult[Option[T]] =
      internalAlternatives(List(col, col.toUpperCase(), col.toLowerCase(), col.capitalize))

    //
    // Try to decode a column with several alternative each expanded to several capitalization
    // If any fails the whole thing fails, if none is found return none
    def withAlternativesM[T](
      defaultValue: Option[T] = None,
      col:          String,
      extras:       String*
    )(using CellDecoder[Option[T]]): DecoderResult[Option[T]] =
      internalAlternatives(
        (col :: extras.toList).flatMap(col =>
          List(col, col.toUpperCase(), col.toLowerCase(), col.capitalize)
        )
      )

  private given CsvRowDecoder[TargetCsvRow, String] =
    (row: CsvRow[String]) =>
      for {
        name              <- row.asIn[NonEmptyString]("Name", "NAME")
        ra                <- row.withAlternativesM[RightAscension](None, "RAJ2000", "RaJ2000")
        dec               <- row.withAlternativesM[Declination](None, "DecJ2000", "DECJ2000")
        pmRa              <- row.withAlternativesM[ProperMotion.RA](None, "pmRa", "pmRA")
        pmDec             <- row.withAlternativesM[ProperMotion.Dec](None, "pmDec", "pmDEC")
        epoch             <- row.withAlternatives[Epoch]("epoch", Epoch.J2000.some)
        parallax          <- row.withAlternatives[Parallax]("parallax", None)
        rv                <- row.withAlternativesM[RadialVelocity](RadialVelocity.Zero.some,
                                                                   "rv",
                                                                   "radialvelocity",
                                                                   "radialVelocity"
                             )
        z                 <- row.withAlternativesM[Redshift](None, "redshift")
        rowBrightnesses    = brightnesses(row)
        rowIntegratedUnits = integratedUnits(row)
        rowSurfaceUnits    = surfaceUnits(row)
        _                 <- if (rowIntegratedUnits.nonEmpty && rowSurfaceUnits.nonEmpty)
                               DecoderError("Cannot mix sourface and integrated units").asLeft
                             else ().asRight
        bare               = ra.isEmpty || dec.isEmpty
      } yield TargetCsvRow(row.line,
                           name,
                           bare,
                           ra,
                           dec,
                           pmRa,
                           pmDec,
                           epoch,
                           rowBrightnesses,
                           rowIntegratedUnits,
                           rowSurfaceUnits,
                           parallax,
                           rv,
                           z
      )

  private def pm(t: TargetCsvRow): Option[ProperMotion] =
    (t.pmRA, t.pmDec) match
      case (Some(ra), Some(dec)) => ProperMotion(ra, dec).some
      case (Some(ra), None)      => ProperMotion(ra, ProperMotion.ZeroDecVelocity).some
      case (None, Some(dec))     => ProperMotion(ProperMotion.ZeroRAVelocity, dec).some
      case _                     => None

  private def tracking(t: TargetCsvRow, ra: RightAscension, dec: Declination): SiderealTracking =
    val base = Coordinates(ra, dec)
    SiderealTracking(base,
                     t.epoch.getOrElse(Epoch.J2000),
                     pm(t),
                     t.rv.orElse(t.z.flatMap(_.toRadialVelocity)).orElse(RadialVelocity.Zero.some),
                     t.parallax
    )

  private def csv2targetsRows[F[_]: RaiseThrowable]: Pipe[F, String, DecoderResult[TargetCsvRow]] =
    in =>
      in
        .through(text.lines)
        .filterNot { s =>
          // Skip comment lines
          s.startsWith("#") || s.startsWith("""//""")
        }
        .intersperse("\n")
        .through(lowlevel.rows[F, String]())
        .through(lowlevel.headers[F, String])
        .through(lowlevel.attemptDecodeRow[F, String, TargetCsvRow])

  def csv2targets[F[_]: RaiseThrowable]
    : Pipe[F, String, EitherNec[ImportProblem, Target.Sidereal]] =
    csv2targetsRows.andThen(
      _.map(t =>
        t.leftMap(e => ImportProblem.CsvParsingError(e.getMessage, e.line))
          .map(t =>
            (t.ra, t.dec)
              .mapN((ra, dec) =>
                Target
                  .Sidereal(name = t.name,
                            tracking = tracking(t, ra, dec),
                            sourceProfile = t.sourceProfile,
                            None
                  )
              )
              .getOrElse(
                Target.Sidereal(
                  t.name,
                  tracking = SiderealTracking.const(Coordinates.Zero).copy(properMotion = pm(t)),
                  sourceProfile = t.sourceProfile,
                  None
                )
              )
          )
          .toEitherNec
      )
    )

  def csv2targetsAndLookup[F[_]: Concurrent](
    client: Client[F],
    proxy:  Option[Uri] = None
  ): Pipe[F, String, EitherNec[ImportProblem, Target.Sidereal]] =
    csv2targetsRows.andThen { t =>
      t.evalMap {
        case Left(e)  =>
          ImportProblem
            .CsvParsingError(e.getMessage, e.line)
            .asLeft[Target.Sidereal]
            .toEitherNec[ImportProblem]
            .pure[F]
        case Right(t) =>
          (t.ra, t.dec)
            .mapN((ra, dec) =>
              // If ra/dec are defined just parse
              Target
                .Sidereal(name = t.name,
                          tracking = tracking(t, ra, dec),
                          sourceProfile = t.sourceProfile,
                          None
                )
                .rightNec
                .pure[F]
            )
            .getOrElse {
              // If only there is name do a lookup
              val queryUri = CatalogSearch.simbadSearchQuery(QueryByName(t.name, proxy))
              val request  = Request[F](GET, queryUri)

              client
                .stream(request)
                .flatMap(
                  _.body
                    .through(text.utf8.decode)
                    .through(CatalogSearch.siderealTargets[F](CatalogAdapter.Simbad))
                )
                .compile
                .toList
                // Convert catalog errors to import errors
                .map(
                  _.map(
                    _.leftMap(e => ImportProblem.LookupError(e.foldMap(_.displayValue), t.line))
                      .leftWiden[ImportProblem]
                  )
                )
                .map(imports =>
                  // Fail if there is more than one result
                  val result: EitherNec[ImportProblem, Target.Sidereal] =
                    if (imports.length === 1) imports.head.map(_.target).toEitherNec
                    else
                      ImportProblem
                        .LookupError(s"Multiple or no matches for ${t.name}", t.line)
                        .leftNec
                  result
                )
                // Handle general errors
                .handleError { e =>
                  ImportProblem
                    .LookupError(e.getMessage, t.line)
                    .asLeft[Target.Sidereal]
                    .toEitherNec
                }
            }

      }
    }
