// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac
package operation

import cats._
import cats.implicits._
import edu.gemini.tac.qengine.api.config._
import edu.gemini.tac.qengine.impl.QueueEngine3
import java.nio.file.Path
import edu.gemini.qengine.skycalc.RaBinSize
import edu.gemini.qengine.skycalc.DecBinSize
import edu.gemini.qengine.skycalc.RaDecBinCalc
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.model.IntCentiPercent
import lucuma.core.data.PerSite
import cats.Applicative
import edu.gemini.tac.qengine.impl.resource.SemesterResource
import edu.gemini.tac.qengine.log.ProposalLog
import edu.gemini.tac.qengine.api.queue.ProposalQueue
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import edu.gemini.tac.qservice.impl.shutdown.ShutdownCalc
import lucuma.core.util.TimeSpan
import edu.gemini.tac.qengine.api.QueueCalc

abstract class AbstractQueueOperation[F[_]: Applicative](
  qe:             QueueEngine3.type,
  siteConfig:     PerSite[Path],
) extends Operation[F] {

  def computeQueue(ws: Workspace[F])(
    implicit ev: FlatMap[F]
  ): F[(List[Proposal], QueueCalc)] =
    for {

      // Load the things we need
      cc <- ws.commonConfig
      qc <- siteConfig.traverse(ws.queueConfig(_))
      ps <- ws.proposals
      // TODO: rollovers

      // Compute the queue
      queueCalc = qe.calc(
        proposals  = ps,
        queueTimes = (band, site) => qc(site).engine.queueTimes(band),
        config     = 
          qc.map: qc =>
            QueueEngineConfig(
              binConfig  = createConfig(
                site       = qc.site,
                semester   = cc.semester,
                ra         = qc.raBinSize,
                dec        = qc.decBinSize,
                shutdowns  = cc.engine.shutdowns(qc.site),
                conditions = cc.engine.conditionsBins
              ),
              timeAccountingCategorySeq = ???,
              restrictedBinConfig = RestrictionConfig(
                relativeTimeRestrictions = Nil, // TODO
                absoluteTimeRestrictions = Nil, // TODO
                bandRestrictions         = Nil, // TODO
              ),
          )
      )

    } yield (ps, queueCalc)

  private def shutdownHours(shutdowns : List[Shutdown], site: Site, semester: Semester, size: RaBinSize): List[TimeSpan] =
    ShutdownCalc.sumHoursPerRa(ShutdownCalc.trim(shutdowns, site, semester), size)

  private def createConfig(site: Site, semester: Semester, ra: RaBinSize, dec: DecBinSize, shutdowns: List[Shutdown], conditions: ConditionsCategoryMap[IntCentiPercent]): SiteSemesterConfig = {
    val calc = RaDecBinCalc.get(site, semester, ra, dec)
    val hrs  = calc.raHours.zip(shutdownHours(shutdowns, site, semester, ra)).map(_.toTimeSpan -| _) 
    val perc = calc.decPercents
    SiteSemesterConfig(
      site       = site,
      semester   = semester,
      raLimits   = RightAscensionMap(hrs),
      decLimits  = DeclinationMap(perc),
      shutdowns  = shutdowns,
      conditions = conditions
    )
  }

}
