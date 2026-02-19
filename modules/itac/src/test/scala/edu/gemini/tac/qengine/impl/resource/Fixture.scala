// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import cats.implicits.*
import edu.gemini.tac.qengine.api.config.*
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Ge
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Le
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.api.queue.time.TimeAccountingCategoryTime
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Half
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.util.Enumerated

import scala.annotation.unused

object Fixture {
  val site = Site.GS
  val semester = Semester(YearInt.unsafeFrom(2011), Half.A)
  val TimeAccountingCategorys = Enumerated[TimeAccountingCategory].all

  // (-90,  0]   0%
  // (  0, 45] 100%
  // ( 45, 90)  50%
  val decBins   = DeclinationMap.fromBins(
    DecRanged( 0, 45, Percent(100)),
    DecRanged(45, 90, Percent( 50)).inclusive
  )

  // <=CC70 50%
  // >=CC80 50%
  val condsBins = ConditionsCategoryMap.ofPercent(
    (ConditionsCategory(Le(CloudExtinction.Preset.PointThree)), 50), (ConditionsCategory(Ge(CloudExtinction.Preset.OnePointZero)), 50)
  )

  // 0 hrs, 1 hrs, 2 hrs, ... 23 hrs
  val raLimits   = RightAscensionMap((0 to 23).map(Time.hours(_)))
  val binConfig  = new SiteSemesterConfig(site, semester, raLimits, decBins, List.empty, condsBins)
  val raResGroup = RightAscensionMapResource(binConfig)

  def compositeTimeRestrictionResource(total: Time): List[TimeRestriction[BoundedTime]] = {
    val bins = RestrictionConfig().mapTimeRestrictions(
      perc => BoundedTime(total * perc),
      _ => BoundedTime(total))
    bins
  }

  def semesterRes(total: Time): SemesterResource =
    new SemesterResource(raResGroup, compositeTimeRestrictionResource(total))

  // Falls in the first conditions bin (<=CC70)
  val goodCC = ObservingConditions(CloudExtinction.Preset.Zero, ImageQuality.Preset.TwoPointZero, SkyBackground.Bright, WVAny)

  // Falls in the second conditions bin (>=CC80)
  val badCC  = ObservingConditions(CloudExtinction.Preset.OnePointZero, ImageQuality.Preset.TwoPointZero, SkyBackground.Bright, WVAny)

  def genQuanta(hrs: Double): TimeAccountingCategoryTime = TimeAccountingCategoryTime.constant(Time.hours(hrs))

  // Makes a proposal with the given ntac info, and observations according
  // to the descriptions (target, conditions, time)
  def mkProp(ntac: Ntac, obsDefs: (Target, ObservingConditions, Time)*): Proposal =
    Proposal(ntac, site = site, obsList = obsDefs.map(tup => Observation(tup._1, tup._2, tup._3)).toList)

  val emptyQueue = ProposalQueueBuilder(QueueTime(TimeAccountingCategoryTime.empty, Percent.Zero), ScienceBand.Band1, Nil) // QueueTime(Site.GN, TimeAccountingCategoryTime.empty(TimeAccountingCategorys).map, TimeAccountingCategorys))
  def evenQueue(hrs: Double): ProposalQueueBuilder =
    evenQueue(hrs, Some(QueueTime.DefaultTimeAccountingCategoryOverfillAllowance))

  // defaults
  val Band1Percent = Percent(30)
  val Band2Percent = Percent(30)
  val Band3Percent = Percent(20)

  def evenQueueTime(hrs: Double, @unused overfill: Option[Percent]): QueueTime = {
    val pt = TimeAccountingCategoryTime.fromFunction { _ => Time.hours(hrs) * Band1Percent }
    QueueTime(pt, Percent.Zero)
  }

  def evenQueue(hrs: Double, overfill: Option[Percent]): ProposalQueueBuilder =
    ProposalQueueBuilder(evenQueueTime(hrs, overfill), ScienceBand.Band1)


}