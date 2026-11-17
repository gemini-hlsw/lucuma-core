// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.ocs

import cats.data.EitherT
import cats.data.StateT
import edu.gemini.tac.qengine.p1.Proposal
import itac.ocs.Namer.State
import lucuma.core.enums.ScienceBand

import java.io.File
import scala.xml.Elem

object Converter:

  def convert(root: Elem, band: ScienceBand, xmlFile: Option[File]): Either[String, Proposal] =
    val xml = ProposalXml2[EitherT[StateT[cats.Id, State, *], String, *]](root, band, xmlFile)
    xml.proposal.value.runA(State.Initial)

