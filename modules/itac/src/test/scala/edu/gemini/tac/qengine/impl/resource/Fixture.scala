// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.resource

import edu.gemini.tac.qengine.api.config.*
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Ge
import edu.gemini.tac.qengine.api.config.ConditionsCategory.Le
import edu.gemini.tac.qengine.api.queue.time.PartnerTime
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.ctx.Partner
import edu.gemini.tac.qengine.impl.queue.ProposalQueueBuilder
import edu.gemini.tac.qengine.p1.*
import edu.gemini.tac.qengine.p1.CloudCover.*
import edu.gemini.tac.qengine.p1.ImageQuality.*
import edu.gemini.tac.qengine.p1.SkyBackground.*
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.BoundedTime
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt
import lucuma.core.util.Enumerated

import scala.annotation.unused

object Fixture {
  val site = Site.GS
  val semester = Semester(YearInt.unsafeFrom(2011), Half.A)
  val partners = Enumerated[Partner].all

  // (-90,  0]   0%
  // (  0, 45] 100%
  // ( 45, 90)  50%
  val decBins   = DecBinGroup.fromBins(
    DecBin( 0, 45, Percent(100)),
    DecBin(45, 90, Percent( 50)).inclusive
  )

  // <=CC70 50%
  // >=CC80 50%
  val condsBins = ConditionsBinGroup.ofPercent(
    (ConditionsCategory(Le(CC70)), 50), (ConditionsCategory(Ge(CC80)), 50)
  )

  // 0 hrs, 1 hrs, 2 hrs, ... 23 hrs
  val raLimits   = RaBinGroup((0 to 23).map(Time.hours(_)))
  val binConfig  = new SiteSemesterConfig(site, semester, raLimits, decBins, List.empty, condsBins)
  val raResGroup = RaResourceGroup(binConfig)

  def timeResourceGroup(total: Time): TimeResourceGroup = {
    val bins = RestrictionConfig().mapTimeRestrictions(
      perc => BoundedTime(total * perc),
      _ => BoundedTime(total))
    new TimeResourceGroup(bins.map(new TimeResource(_)))
  }

  def semesterRes(total: Time): SemesterResource =
    new SemesterResource(raResGroup, timeResourceGroup(total), QueueBand.QBand1)

  // Falls in the first conditions bin (<=CC70)
  val goodCC = ObservingConditions(CC50, IQAny, SBAny, WVAny)

  // Falls in the second conditions bin (>=CC80)
  val badCC  = ObservingConditions(CC80, IQAny, SBAny, WVAny)

  def genQuanta(hrs: Double): PartnerTime = PartnerTime.constant(Time.hours(hrs))

  // Makes a proposal with the given ntac info, and observations according
  // to the descriptions (target, conditions, time)
  def mkProp(ntac: Ntac, obsDefs: (Target, ObservingConditions, Time)*): Proposal =
    Proposal(ntac, site = site, obsList = obsDefs.map(tup => Observation(null, tup._1, tup._2, tup._3)).toList)

  val emptyQueue = ProposalQueueBuilder(QueueTime(PartnerTime.empty, Percent.Zero), QueueBand.QBand1, Nil) // QueueTime(Site.GN, PartnerTime.empty(partners).map, partners))
  def evenQueue(hrs: Double): ProposalQueueBuilder =
    evenQueue(hrs, Some(QueueTime.DefaultPartnerOverfillAllowance))

  // defaults
  val Band1Percent = Percent(30)
  val Band2Percent = Percent(30)
  val Band3Percent = Percent(20)

  def evenQueueTime(hrs: Double, @unused overfill: Option[Percent]): QueueTime = {
    val pt = PartnerTime.fromFunction { _ => Time.hours(hrs) * Band1Percent }
    QueueTime(pt, Percent.Zero)
  }

  def evenQueue(hrs: Double, overfill: Option[Percent]): ProposalQueueBuilder =
    ProposalQueueBuilder(evenQueueTime(hrs, overfill), QueueBand.QBand1)


}