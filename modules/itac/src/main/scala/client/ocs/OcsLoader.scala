// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.ocs

import cats.data.EitherT
import cats.data.StateT
import cats.effect.Sync
import cats.syntax.all.*
import edu.gemini.tac.qengine.p1.Proposal
import itac.ocs.Namer.State
import lucuma.core.enums.ScienceBand
import org.typelevel.log4cats.Logger

import java.io.File
import java.nio.file.Path
import scala.xml.Elem
import scala.xml.XML

trait OcsLoader[F[_]]:
  def loadProposal(log: Logger[F], band: ScienceBand, file: File): F[Either[String, Proposal]]
  def loadProposals(log: Logger[F], band: ScienceBand, dir: Path): F[Either[String, List[Proposal]]]

object OcsLoader:
  
  def apply[F[_]: Sync]: OcsLoader[F] =
    new OcsLoader[F]:

      def loadProposals(log: Logger[F], band: ScienceBand, dir: Path): F[Either[String, List[Proposal]]] =
        log.debug(s"OcsLoader: Loading band ${band.intValue} proposals from $dir") >>
        Sync[F].blocking(dir.toFile.listFiles.toList.filter(_.getName().endsWith(".xml"))).flatMap: fs =>
          fs.traverse(loadProposal(log, band, _)).map(_.sequence)

      def loadProposal(log: Logger[F], band: ScienceBand, file: File): F[Either[String, Proposal]] =
        log.debug(s"OcsLoader: Loading band ${band.intValue} proposal from $file") >>
        Sync[F].blocking:
          convert(XML.load(file), band)

      private def convert(root: Elem, band: ScienceBand): Either[String, Proposal] =
        val xml = ProposalXml2[EitherT[StateT[cats.Id, State, *], String, *]](root, band)
        xml.proposal.value.runA(State.Initial)
