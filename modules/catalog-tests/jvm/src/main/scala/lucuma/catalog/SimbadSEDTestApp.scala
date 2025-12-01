// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import cats.effect.*
import cats.syntax.all.*
import fs2.*
import lucuma.catalog.*
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import org.http4s.Method.*
import org.http4s.Request
import org.http4s.client.Client
import org.http4s.jdkhttpclient.JdkHttpClient

import scala.io.Source

/**
 * Application that queries Simbad for a list of targets and displays SED inference results.
 * Replicates the functionality of match_sed_test.py from the Python reference implementation.
 *
 * Usage: sbt "catalogTestsJVM/runMain lucuma.catalog.votable.SimbadSEDTestApp Vega Sirius" sbt
 * "catalogTestsJVM/runMain lucuma.catalog.votable.SimbadSEDTestApp --file targets.txt"
 */
object SimbadSEDTestApp extends IOApp:

  case class TargetResult(
    name:         String,
    catalogInfo:  Option[String],
    otype:        Option[String],
    spectralType: Option[String],
    morphType:    Option[String],
    sed:          Option[UnnormalizedSED],
    error:        Option[String]
  )

  override def run(args: List[String]): IO[ExitCode] =
    val targets =
      if args.headOption.contains("--file") then
        // Read from file
        args.drop(1).headOption match
          case Some(filename) =>
            val source = Source.fromFile(filename)
            try source.getLines().filterNot(_.trim.isEmpty).toList
            finally source.close()
          case None           =>
            System.err.println("Error: --file requires a filename argument")
            List.empty
      else if args.isEmpty then
        // Default test targets
        List("Vega", "Sirius", "Betelgeuse", "M 31", "M 87", "3C 273", "M 42", "NGC 7009")
      else
        // Targets from command line
        args

    if targets.isEmpty then
      IO.println(
        "Usage: SimbadSEDTestApp [target1 target2 ...] or SimbadSEDTestApp --file targets.txt"
      ) *>
        IO.pure(ExitCode.Error)
    else
      JdkHttpClient.simple[IO].use { client =>
        for
          _       <- IO.println(s"\nQuerying ${targets.length} targets from Simbad...\n")
          results <- targets.traverse(queryTarget(client, _))
          _       <- displayResults(results)
        yield ExitCode.Success
      }

  private def queryTarget(client: Client[IO], targetName: String): IO[TargetResult] =
    import eu.timepit.refined.refineV
    import eu.timepit.refined.collection.NonEmpty

    refineV[NonEmpty](targetName) match
      case Right(refined) =>
        val request = Request[IO](GET, CatalogSearch.simbadSearchQuery(QueryByName(refined)))
        client
          .stream(request)
          .flatMap(
            _.body
              .through(text.utf8.decode)
              .through(CatalogSearch.siderealTargets[IO](CatalogAdapter.Simbad))
          )
          .compile
          .toList
          .flatMap {
            case result :: _ => IO.pure(extractInfo(targetName, result))
            case Nil         =>
              IO.pure(TargetResult(targetName, None, None, None, None, None, Some("Not found")))
          }
          .handleErrorWith { e =>
            IO.pure(TargetResult(targetName, None, None, None, None, None, Some(e.getMessage)))
          }
      case Left(error)    =>
        IO.pure(
          TargetResult(targetName,
                       None,
                       None,
                       None,
                       None,
                       None,
                       Some(s"Invalid target name: $error")
          )
        )

  private def extractInfo(
    searchName: String,
    result:     Either[cats.data.NonEmptyChain[CatalogProblem], CatalogTargetResult]
  ): TargetResult =
    result match
      case Right(CatalogTargetResult(target, _)) =>
        val catalogInfoOpt = target.catalogInfo
        val otype          = catalogInfoOpt.flatMap(_.objectType.map(_.value))
        val morphType      = None // Not directly available from Target
        val spectralType   = None // Not directly available from Target

        // Extract SED from target
        val sed = target.sourceProfile match
          case lucuma.core.model.SourceProfile.Point(spectralDef) =>
            spectralDef match
              case lucuma.core.model.SpectralDefinition.BandNormalized(sedOpt, _) => sedOpt
              case _                                                              => None
          case _                                                  => None

        val catalogInfoStr = catalogInfoOpt.map { ci =>
          s"${ci.id} (${ci.objectType.map(_.value).getOrElse("?")})"
        }

        TargetResult(
          searchName,
          catalogInfoStr,
          otype,
          spectralType,
          morphType,
          sed,
          None
        )

      case Left(errors) =>
        TargetResult(searchName, None, None, None, None, None, Some(errors.toList.mkString(", ")))

  private def displayResults(results: List[TargetResult]): IO[Unit] =
    // Calculate column widths
    val nameWidth  = (results.map(_.name.length) :+ 20).max
    val infoWidth  = (results.map(_.catalogInfo.map(_.length).getOrElse(0)) :+ 30).max
    val otypeWidth = (results.map(_.otype.map(_.length).getOrElse(0)) :+ 15).max
    val sedWidth   = 40

    // Print header
    val header    =
      s"${"Name".padTo(nameWidth, ' ')} | ${"Catalog Info".padTo(infoWidth, ' ')} | ${"OTYPE".padTo(otypeWidth, ' ')} | ${"Inferred SED".padTo(sedWidth, ' ')}"
    val separator = "-" * (nameWidth + infoWidth + otypeWidth + sedWidth + 12)

    IO.println(header) *>
      IO.println(separator) *>
      results.traverse_ { result =>
        val name   = result.name.padTo(nameWidth, ' ')
        val info   = result.catalogInfo.getOrElse("-").padTo(infoWidth, ' ')
        val otype  = result.otype.getOrElse("-").padTo(otypeWidth, ' ')
        val sed    = result.sed match
          case Some(s) => s.toString.take(sedWidth)
          case None    => result.error.getOrElse("No SED inferred")
        val sedStr = sed.padTo(sedWidth, ' ')

        IO.println(s"$name | $info | $otype | $sedStr")
      } *>
      IO.println("") *>
      displaySummary(results)

  private def displaySummary(results: List[TargetResult]): IO[Unit] =
    val total     = results.length
    val matched   = results.count(_.sed.isDefined)
    val errors    = results.count(_.error.isDefined)
    val unmatched = total - matched - errors

    val sedTypes = results
      .flatMap(_.sed)
      .groupBy {
        case UnnormalizedSED.StellarLibrary(_)  => "Stellar"
        case UnnormalizedSED.Galaxy(_)          => "Galaxy"
        case UnnormalizedSED.Quasar(_)          => "Quasar"
        case UnnormalizedSED.HIIRegion(_)       => "HII Region"
        case UnnormalizedSED.PlanetaryNebula(_) => "Planetary Nebula"
        case UnnormalizedSED.PowerLaw(_)        => "Power Law"
        case UnnormalizedSED.BlackBody(_)       => "Black Body"
        case UnnormalizedSED.UserDefined(_)     => "User Defined"
      }
      .view
      .mapValues(_.length)
      .toMap

    IO.println("=== Summary ===") *>
      IO.println(s"Total targets: $total") *>
      IO.println(s"SEDs matched: $matched (${if total > 0 then matched * 100 / total else 0}%)") *>
      IO.println(s"No SED: $unmatched (${if total > 0 then unmatched * 100 / total else 0}%)") *>
      IO.println(s"Errors: $errors") *>
      IO.println("") *>
      IO.println("SED types:") *>
      sedTypes.toList.sortBy(_._1).traverse_ { case (sedType, count) =>
        IO.println(s"  $sedType: $count")
      }
