// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import cats.implicits.*
import edu.gemini.tac.qengine.api.config.ConditionsCategory
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Ge
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Le
import edu.gemini.tac.qengine.api.config.ConditionsCategoryMap
import edu.gemini.tac.qengine.api.config.DecRanged
import edu.gemini.tac.qengine.api.config.DeclinationMap
import edu.gemini.tac.qengine.api.config.QueueEngineConfig
import edu.gemini.tac.qengine.api.config.RightAscensionMap
import edu.gemini.tac.qengine.api.config.SiteSemesterConfig
import edu.gemini.tac.qengine.api.config.TimeAccountingCategorySequence
import edu.gemini.tac.qengine.impl.QueueEngine3
import edu.gemini.tac.qengine.impl.resource.Fixture
import lucuma.core.data.PerSite
import lucuma.core.enums.Half
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.util.TimeSpan
import ocs.Fixture_25B
import lucuma.core.model.IntCentiPercentUnbounded
import lucuma.core.enums.ScienceBand
import lucuma.core.util.Enumerated
import munit.CatsEffectSuite

class QueueEngineSuite extends CatsEffectSuite:

  val semester = Semester(YearInt.unsafeFrom(2025), Half.B)

  // (-90,  0]   0%
  // (  0, 45] 100%
  // ( 45, 90)  50%
  val decBins   = DeclinationMap.fromBins(
    DecRanged( 0, 45, IntCentiPercentUnbounded.unsafeFromPercent(100)),
    DecRanged(45, 90, IntCentiPercentUnbounded.unsafeFromPercent( 50)).inclusive
  )

  // <=CC70 50%
  // >=CC80 50%
  val condsBins = ConditionsCategoryMap.ofPercent(
    (ConditionsCategory(Le(CloudExtinction.Preset.PointThree)), 50), 
    (ConditionsCategory(Ge(CloudExtinction.Preset.OnePointZero)), 50)
  )

  // 0 hrs, 1 hrs, 2 hrs, ... 23 hrs
  val raLimits   = RightAscensionMap.gen1HrBins((_, _) => TimeSpan.fromHoursBounded(100))
  val binConfig  = PerSite.unfold(new SiteSemesterConfig(_, semester, raLimits, decBins, List.empty, condsBins))

  val seq = new TimeAccountingCategorySequence:
    def sequence: LazyList[TimeAccountingCategory] =
      TimeAccountingCategory.values.to(LazyList) #::: sequence

  def cfg = binConfig.map(QueueEngineConfig(_, seq))

  test("foo"):

    Fixture_25B.loadAll.flatMap: loaded =>
      println(loaded)
      val qt = Fixture.evenQueueTime(1000, None) // TODO: do this ourselves, this is wrong

      val calc = QueueEngine3.calc(
        loaded.toOption.get,
        (_, _) => qt, 
        cfg
      )

      cats.effect.IO.blocking:
        calc.queues.toList.foreach: queues =>
          Enumerated[ScienceBand].all.map(queues).foreach: q =>
            println()
            println(s"${q.band} at ${q.site}:")
            println()
            println(s"    \tAvailable\tUsed\t\tRemaining")
            TimeAccountingCategory.values.foreach: tac =>
              println(s"  $tac\t${q.queueTime(tac).toHours}\t${q.usedTime(tac).toHours}\t${q.remainingTime(tac).toHours}")

            println()
            q.toList.foreach: ps =>
              println(s"  ${ps.reference} ${ps.parentProposal.reference} rank ${ps.parentProposal.ranking}")

        println()
        // log.toDetailList.foreach: e =>
        //   println(s"${e.key.id}: ${e.msg}")

        println("done")


