// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ocs

import cats.syntax.all.*
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.enums.ScienceBand
import lucuma.core.util.Enumerated
import itac.ocs.OcsLoader
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.effect.IO
import java.nio.file.Path

object Fixture_25B:

  val log = Slf4jLogger.getLogger[IO]

  def loadBand(band: ScienceBand): IO[Either[String, List[Proposal]]] =
    val dir = Path.of(s"/Users/rob.norris/Gemini/ocs/itac/itac_WD/band-${band.intValue}/")
    OcsLoader[IO].loadProposals(log, band, dir)

  def loadAll: IO[Either[String, List[Proposal]]] =
    Enumerated[ScienceBand]
      .all
      .traverse(loadBand)
      .map(_.combineAll)


