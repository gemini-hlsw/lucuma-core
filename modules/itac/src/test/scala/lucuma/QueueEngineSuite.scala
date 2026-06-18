// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import munit.FunSuite
import edu.gemini.tac.qengine.impl.resource.Fixture
import edu.gemini.tac.qengine.api.config.QueueEngineConfig
import edu.gemini.tac.qengine.api.config.TimeAccountingCategorySequence
import lucuma.core.enums.TimeAccountingCategory
import edu.gemini.tac.qengine.p1.Proposal
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.enums.Half
import eu.timepit.refined.types.numeric.PosInt
import cats.data.NonEmptyList
import lucuma.core.model.Allocation
import lucuma.core.enums.ScienceBand
import lucuma.core.util.TimeSpan
import edu.gemini.tac.qengine.p1.ItacObservation
import edu.gemini.tac.qengine.p1.ItacTarget
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.model.CloudExtinction
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ElevationRange
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ProposalType
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.core.model.PartnerSplit
import lucuma.core.enums.Partner
import edu.gemini.tac.qengine.p1.GroupTree
import lucuma.core.model.Observation
import lucuma.core.model.Group
import edu.gemini.tac.qengine.impl.QueueEngine3

class QueueEngineSuite extends FunSuite:

  val refs = Iterator.from(1).map(PosInt.unsafeFrom(_)).map(ProposalReference(Semester(YearInt.unsafeFrom(2026), Half.A), _))
  val oids = Iterator.from(1).map(PosLong.unsafeFrom(_)).map(Observation.Id(_))
  val gids = Iterator.from(1).map(PosLong.unsafeFrom(_)).map(Group.Id(_))

  val target: ItacTarget =
    ItacTarget(Coordinates.unsafeFromRadians(1,1.5), Target.Id(PosLong.unsafeFrom(1)))

  def prop(index: Int): Proposal =
    Proposal(
      ProposalReference(Semester(YearInt.unsafeFrom(2026), Half.A), PosInt.unsafeFrom(index)),
      NonEmptyList.of(
        Allocation(TimeAccountingCategory.US, ScienceBand.Band1, TimeSpan.fromHoursBounded(0.25)),
        Allocation(TimeAccountingCategory.US, ScienceBand.Band2, TimeSpan.fromHoursBounded(0.25)),
      ),
      ProposalType.Queue(ToOActivation.None, IntPercent.unsafeFrom(50), List(
        PartnerSplit(Partner.US, IntPercent.unsafeFrom(100))
      )),
      GroupTree.from(
        ItacObservation(
          target,
          ConstraintSet(
            ImageQuality.Preset.OnePointZero,
            CloudExtinction.Preset.PointFive,
            SkyBackground.Dark,
            WaterVapor.Median,
            ElevationRange.ByAirMass.Default
          ),
          TimeSpan.fromHoursBounded(0.5),
          false,
          ObservingModeType.Flamingos2LongSlit
        )
      )
    )

  test("foo"):

    val qt = Fixture.evenQueueTime(10, None) // TODO: do this ourselves, this is wrong
    val seq = new TimeAccountingCategorySequence:
      def sequence: LazyList[TimeAccountingCategory] =
        TimeAccountingCategory.values.to(LazyList) #::: sequence

    val cfg = QueueEngineConfig(Fixture.binConfig, seq)

    val (resource, log, queues) = QueueEngine3.calc(
      List(
        prop(1),
        prop(2),
      ), 
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


