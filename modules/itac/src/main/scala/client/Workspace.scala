// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac

import cats.effect.Sync
import cats.implicits._
import cats.Parallel
import edu.gemini.tac.qengine.p1.Proposal
import io.circe.{ Encoder, Decoder, DecodingFailure }
import io.circe.yaml.Printer
import io.circe.CursorOp.DownField
import io.circe.syntax._
import io.circe.yaml.parser
import itac.config.Common
import itac.config.QueueConfig
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.nio.file.Paths
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.time.ZonedDateTime
import java.time.ZoneId
import java.io.File
import cats.data.NonEmptyList
import scala.jdk.CollectionConverters._
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import cats.effect.kernel.Ref
import org.typelevel.log4cats.Logger
import lucuma.core.util.Enumerated

/** Interface for some Workspace operations. */
trait Workspace[F[_]] {

  def cwd: F[Path]

  /** True if the working directory is empty. */
  def isEmpty: F[Boolean]

  /** Write an encodable value to a file. Header must be a YAML comment. */
  def writeData[A: Encoder](path: Path, a: A, header: String = ""): F[Path]

  /** Read a decodable value from the specified file. */
  def readData[A: Decoder](path: Path): F[A]

  def readText(path: Path): F[String]

  /**
   * Create a directory relative to `cwd`, including any intermediate ones, returning the path of
   * the created directory.
   */
  // def mkdirs(path: Path): F[Path]
  def commonConfig: F[Common]
  def queueConfig(path: Path): F[QueueConfig]
  def proposals: F[List[Proposal]]
  // def removed: F[List[Proposal]]

  def proposal(ref: String): F[(File, NonEmptyList[Proposal])]

  /**
   * Create a directory under `cwd` with a name like GN-20190524-103322 and return its path
   * relative to the workspace root.
   */
  // def newQueueFolder(site: Site): F[Path]

  // def writeRolloveReport(path: Path, rr: RolloverReport): F[Path]

  // def readRolloverReport(path: Path): F[RolloverReport]
  def writeText(path: Path, text: String): F[Path]

  // def progIdHash: F[ProgIdHash]

  // def bulkEdits(ps: List[Proposal]): F[Map[String, BulkEdit]]

}

object Workspace {

  // val RemovedDir            = Paths.get("removed")
  // val EditsDir              = Paths.get("edits")
  // val BulkEditsFile         = Paths.get("bulk_edits.xls")

  def proposalDir(band: ScienceBand): Path = ??? //Paths.get(s"band-${band.number}")

  // val WorkspaceDirs: List[Path] =
  //   List(RemovedDir, EditsDir) ++ ScienceBand.values.map(proposalDir)

  object Default {
    val CommonConfigFile = Paths.get("common.yaml")
    def queueConfigFile(site: Site) = Paths.get(s"${site.shortName.toLowerCase}-queue.yaml")
    def rolloverReport(site: Site) = Paths.get(s"${site.shortName.toLowerCase}-rollovers.yaml")
  }

  val printer: Printer =
    Printer(
      preserveOrder = true,
      // dropNullKeys = false,
      // indent = 2,
      maxScalarWidth = 120,
      // splitLines = true,
      // indicatorIndent = 0,
      // tags = Map.empty,
      // sequenceStyle = FlowStyle.Block,
      // mappingStyle = FlowStyle.Block,
      // stringStyle = StringStyle.Plain,
      // lineBreak = LineBreak.Unix,
      // explicitStart = false,
      // explicitEnd = false,
      // version = YamlVersion.Auto
    )

  def apply[F[_]: Sync: Parallel](dir: Path, cc: Path, log: Logger[F], force: Boolean): F[Workspace[F]] =
    ItacException(s"Workspace directory not found: ${dir}").raiseError[F, Workspace[F]].unlessA(dir.toFile.getAbsoluteFile.isDirectory) *>
    log.info(s"Working directory is ${dir.toAbsolutePath()}") >>
    Ref[F].of(Map.empty[Path, String]).map { cache =>
      new Workspace[F] {
        given Logger[F] = log

        def cwd = dir.pure[F]

        def isEmpty: F[Boolean] =
          Sync[F].delay(Option(dir.toFile.getAbsoluteFile.listFiles).foldMap(_.toList).isEmpty)

        def readText(path: Path): F[String] =
          cache.get.flatMap { map =>
            val p = dir.resolve(path)
            map.get(path) match {
              case Some(a) => log.debug(s"Getting $p from cache.").as(a)
              case None    => log.debug(s"Reading: $p") *>
                Sync[F].delay(new String(Files.readAllBytes(p), "UTF-8"))
                  .flatTap(text =>  cache.set(map + (path -> text)))
                  .onError {
                    case _: NoSuchFileException =>
                      ItacException(s"No such file: $path").raiseError[F, Unit]
                  }
            }
          }

        def readData[A: Decoder](path: Path): F[A] =
          readText(path)
            .map(parser.parse(_))
            .flatMap {
              _.leftMap(ex => ItacException(s"Failure reading $path\n$ex.message"))
               .flatMap(Decoder[A].decodeJson)
               .liftTo[F]
            }
            .onError {
              case f: DecodingFailure =>
                Sync[F].raiseError(
                  ItacException(s"""|Failure reading $path\n  ${f.message}
                                    |    at ${f.history.collect { case DownField(k) => k } mkString("/")}
                                    |""".stripMargin.trim)
                )
            }

        def listFiles(path: Path): F[List[Path]] =
          Sync[F].delay {
            Files
              .list(dir.resolve(path))
              .iterator
              .asScala
              .map(dir.relativize)
              .toList
          }

        def readAll[A: Decoder](path: Path, suffix: String): F[List[A]] =
          listFiles(path).flatMap(_.filter(_.toFile.getName.endsWith(suffix)).traverse(readData[A]))

        def writeText(path: Path, text: String): F[Path] = {
          val p = dir.resolve(path)
          Sync[F].delay(p.toFile.getAbsoluteFile.isFile).flatMap {
            case false | `force` => log.info(s"Writing: $p") *>
              Sync[F].delay(Files.write(dir.resolve(path), text.getBytes("UTF-8")))
            case true  => Sync[F].raiseError(ItacException(s"File exists: $p"))
          }
        }

        def writeData[A: Encoder](path: Path, a: A, header: String = ""): F[Path] =
          writeText(path, header + printer.pretty(a.asJson))

        // def writeRolloveReport(path: Path, rr: RolloverReport): F[Path] = {
        //   // The user cares about when report was generated in local time, so that's what we will
        //   // use in the header comment.
        //   val ldt = ZonedDateTime.ofInstant(rr.timestamp, ZoneId.systemDefault)
        //   val fmt = DateTimeFormatter.ofPattern("yyyy-dd-MM HH:mm 'local time' (z)")
        //   val header =
        //     s"""|
        //         |# This is the ${rr.semester} rollover report for ${rr.site.displayName}, generated at ${fmt.format(ldt)}.
        //         |# It is ok to edit this file as long as the format is preserved, and it is ok to add comment lines.
        //         |
        //         |""".stripMargin
        //   writeData(path, rr, header)
        // }

        // def readRolloverReport(path: Path): F[RolloverReport] =
        //   readData(path)(decoderRolloverReport)

        // def mkdirs(path: Path): F[Path] = {
        //   val p = dir.resolve(path)
        //   Sync[F].delay(p.toFile.getAbsoluteFile.isDirectory) flatMap {
        //     case true  => log.debug(s"Already exists: $path").as(p)
        //     case false => log.info(s"Creating folder: $path") *>
        //       Sync[F].delay(Files.createDirectories(dir.resolve(path)))
        //   }
        // }

        def commonConfig: F[Common] = ???
          readData[Common](cc).recoverWith {
            case _: NoSuchFileException => cwd.flatMap(p => Sync[F].raiseError(ItacException(s"Not an ITAC Workspace: $p")))
          }

        def queueConfig(path: Path): F[QueueConfig] = ???
        //   readData[QueueConfig](path).recoverWith {
        //     case _: NoSuchFileException => cwd.flatMap(p => Sync[F].raiseError(ItacException(s"Site-specific configuration file not found: ${p.resolve(path)}")))
        //   }

        // def edits: F[Map[String, SummaryEdit]] =
        //   readAll[SummaryEdit](EditsDir, ".yaml").map(_.map(e => (e.reference -> e)).toMap)

        // def bulkEdits(ps: List[Proposal]): F[Map[String, BulkEdit]] =
        //   for {
        //     f <- cwd.map(_.resolve(BulkEditsFile).toFile.getAbsoluteFile)
        //     _ <- log.debug(s"Creating/updating and then reading bulk edits from ${f.getAbsoluteFile()}")
        //     _ <- BulkEditFile.createOrUpdate(f, ps)
        //     m <- BulkEditFile.read(f)
        //   } yield m

        def loadProposals(dir: Path): F[List[Proposal]] =
          ???
          // for {
          //   cwd  <- cwd
          //   conf <- commonConfig
          //   p     = cwd.resolve(dir)
          //   when  = conf.semester.getMidpointDate(Site.GN).getTime // arbitrary
          //   _    <- log.debug(s"Reading proposals from $p")
          //   es   <- edits
          //   ps   <- ProposalLoader[F](when, es, log, mutator).loadMany(p.toFile.getAbsoluteFile)
          //   _    <- ps.traverse { case (f, Left(es)) => log.warn(s"${f.getName}: ${es.toList.mkString(", ")}") ; case _ => ().pure[F] }
          //   psʹ   = ps.collect { case (_, Right(ps)) => ps.toList } .flatten
          //   _    <- log.debug(s"Read ${ps.length} proposals.")
          //   ret   = ps.collect { case (_, Right(ps)) => ps.toList } .flatten
          // } yield ret

        def bandedProposals: F[Map[ScienceBand, List[Proposal]]] =
          Enumerated[ScienceBand]
            .all
            .traverse: b => 
              loadProposals(proposalDir(b))
                .map: ps =>
                  Map(b -> ps)
            .map: maps =>
              maps.combineAll

        def proposals: F[List[Proposal]] = bandedProposals.map(_.values.toList.flatten)

        // def removed: F[List[Proposal]] = loadProposals(RemovedDir)

        def proposal(ref: String, band: ScienceBand): F[(File, NonEmptyList[Proposal])] =
          ???
          // for {
          //   cwd  <- cwd
          //   conf <- commonConfig
          //   p     = cwd.resolve(proposalDir(band))
          //   when  = conf.semester.getMidpointDate(Site.GN).getTime // arbitrary
          //   _    <- log.debug(s"Reading proposals from $p")
          //   es   <- edits
          //   p    <- ProposalLoader[F](when, es, log, (_, _) => ().pure[F]).loadByReference(p.toFile.getAbsoluteFile, ref)
          // } yield p

        def proposal(ref: String): F[(File, NonEmptyList[Proposal])] =
          proposal(ref, ScienceBand.Band1) orElse
          proposal(ref, ScienceBand.Band2) orElse
          proposal(ref, ScienceBand.Band3) orElse
          proposal(ref, ScienceBand.Band4)

        // def newQueueFolder(site: Site): F[Path] =
        //   for {
        //     dt <- Sync[F].delay(LocalDateTime.now)
        //     f   = DateTimeFormatter.ofPattern("yyyyMMdd-HHmms")
        //     n   = s"${site.shiort}-${f.format(dt)}"
        //     p   = Paths.get(n)
        //     _  <- mkdirs(p)
        //   } yield p

        // def progIdHash: F[ProgIdHash] =
        //   commonConfig.map(c => new ProgIdHash(c.emailConfig.hashKey))

      }
    }

  // // Wee wrapper class for edits … may move this out.
  // private case class Edits(edits: Option[Map[String, Edit]]) // allow for empty map
  // private object Edits {
  //   implicit val DecoderEdits: Decoder[Edits] = io.circe.generic.semiauto.deriveDecoder
  // }

}