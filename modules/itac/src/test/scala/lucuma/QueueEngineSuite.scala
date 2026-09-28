// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import edu.gemini.tac.qengine.api.config.QueueEngineConfig
import edu.gemini.tac.qengine.api.config.TimeAccountingCategorySequence
import edu.gemini.tac.qengine.impl.QueueEngine3
import edu.gemini.tac.qengine.impl.resource.Fixture
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.TimeSpan
import munit.FunSuite
import lucuma.ocs.Fixture_25B

class QueueEngineSuite extends FunSuite:

  test("foo"):

    val qt = Fixture.evenQueueTime(1000, None) // TODO: do this ourselves, this is wrong

    val seq = new TimeAccountingCategorySequence:
      def sequence: LazyList[TimeAccountingCategory] =
        TimeAccountingCategory.values.to(LazyList) #::: sequence


    val cfg = QueueEngineConfig(Fixture.binConfig, seq)

    println(cfg)

    val (resource, log, queues) = QueueEngine3.calc(
      Fixture_25B.loadAll().toOption.get,
      (_, _) => qt, 
      cfg
    )

    queues.foreach: q =>
      println()
      println(s"${q.band} at ${q.site}:")
      println()
      println(s"    \tAvailable\tUsed\t\tRemaining")
      TimeAccountingCategory.values.foreach: tac =>
        println(s"  $tac\t${q.queueTime(tac).toHours}\t${q.usedTime(tac).toHours}\t${q.remainingTime(tac).toHours}")

      q.toList.foreach: ps =>
        println(s"  ${ps.reference}")

    println()
    log.toDetailList.foreach: e =>
      println(s"${e.key.id}: ${e.msg}")

    println()


