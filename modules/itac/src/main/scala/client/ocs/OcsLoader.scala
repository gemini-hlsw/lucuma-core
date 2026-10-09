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

import java.nio.file.Path
import scala.xml.Elem
import scala.xml.XML

trait OcsLoader[F[_]]:
  def loadProposals(log: Logger[F], dirs: Map[ScienceBand, Path]): F[Either[String, List[Proposal]]]

object OcsLoader:
  
  def apply[F[_]: Sync]: OcsLoader[F] =
    new OcsLoader[F]:

      def loadXmlFiles(dir: Path): F[List[Elem]] =
        Sync[F].blocking:
          if dir.toFile.isDirectory then
            dir.toFile.listFiles.toList.filter(_.getName().endsWith(".xml")).map(XML.load(_))
          else Nil

      def loadProposals(log: Logger[F], dirs: Map[ScienceBand, Path]): F[Either[String, List[Proposal]]] =
        dirs
          .toList
          .flatTraverse: (band, dir) =>
            loadXmlFiles(dir).map(_.tupleRight(band))  
          .map(convertMany(_).value.runA(State.Initial))

      private def convert1(root: Elem, band: ScienceBand): EitherT[StateT[cats.Id, State, _], String, Proposal] =
        ProposalXml2[EitherT[StateT[cats.Id, State, *], String, *]](root, band).proposal

      private def convertMany(xmls: List[(Elem, ScienceBand)]): EitherT[StateT[cats.Id, State, _], String, List[Proposal]]  =
        xmls.traverse(convert1)