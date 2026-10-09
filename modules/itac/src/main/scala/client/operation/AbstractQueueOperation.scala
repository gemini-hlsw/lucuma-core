// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac
package operation

import cats.*
import cats.Applicative
import cats.implicits.*
import edu.gemini.qengine.skycalc.DecBinSize
import edu.gemini.qengine.skycalc.RaBinSize
import edu.gemini.qengine.skycalc.RaDecBinCalc
import edu.gemini.tac.qengine.api.QueueCalc
import edu.gemini.tac.qengine.api.config.*
import edu.gemini.tac.qengine.impl.QueueEngine3
import edu.gemini.tac.qengine.p1.Proposal
import edu.gemini.tac.qservice.impl.shutdown.ShutdownCalc
import lucuma.core.data.PerSite
import lucuma.core.enums.Site
import lucuma.core.model.IntCentiPercent
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan

import java.nio.file.Path

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
              timeAccountingCategorySeq = cc.engine.partnerSequence(qc.site),
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
