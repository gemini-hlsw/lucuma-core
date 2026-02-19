// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import cats.implicits.*
import edu.gemini.tac.qengine.api.config.ConditionsCategory
import edu.gemini.tac.qengine.p1.ObservingConditions
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.SkyBackground
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.core.util.Enumerated

object Default {
  import ConditionsCategory.*
  
  val Conditions = ConditionsCategoryMap.ofPercent(
    (ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.PointOne), Le(SkyBackground.Dark), UnspecifiedWV, Some("1")), 4),
    (ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.PointOne), Ge(SkyBackground.Gray), UnspecifiedWV, Some("2")), 4),
    (ConditionsCategory(Ge(CloudExtinction.Preset.PointThree), Eq(ImageQuality.Preset.PointOne), UnspecifiedSB, UnspecifiedWV, Some("3")), 3),
    (ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.OnePointZero), Le(SkyBackground.Dark), UnspecifiedWV, Some("4")), 10),
    (ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.OnePointZero), Ge(SkyBackground.Gray), UnspecifiedWV, Some("5")), 10),
    (ConditionsCategory(Ge(CloudExtinction.Preset.PointThree), Eq(ImageQuality.Preset.OnePointZero), UnspecifiedSB, UnspecifiedWV, Some("6")), 10),
    (ConditionsCategory(Eq(CloudExtinction.Preset.Zero), Eq(ImageQuality.Preset.OnePointFive), UnspecifiedSB, UnspecifiedWV, Some("7")), 15),
    (ConditionsCategory(Ge(CloudExtinction.Preset.PointThree), Eq(ImageQuality.Preset.OnePointFive), UnspecifiedSB, UnspecifiedWV, Some("8")), 20),
    (ConditionsCategory(UnspecifiedCC, Eq(ImageQuality.Preset.TwoPointZero), UnspecifiedSB, UnspecifiedWV, Some("9")), 40)
  )

  val WvTimeRestriction  = TimeRestriction.wv(Percent(50), WV50)
  val LgsTimeRestriction = TimeRestriction.lgs(Time.hours(200))

  val RapidTooBandRestriction = BandRestriction.rapidToo
  val LgsBandRestriction      = BandRestriction.lgs
  val Iq20BandRestriction     = BandRestriction.iq20
  val NotBand3Restriction     = BandRestriction.notBand3

  val RelativeTimeRestrictions = List(WvTimeRestriction)
  val AbsoluteTimeRestrictions = List(LgsTimeRestriction)
  val BandRestrictions =
    List(NotBand3Restriction, RapidTooBandRestriction, LgsBandRestriction, Iq20BandRestriction)

  def main(args: Array[String]): Unit = {
    Conditions.searchPath.cats foreach { cat =>
      println(cat)
    }

    println("\n\n")

    val ocList = for {
      iq <- Enumerated[ImageQuality.Preset].all
      cc <- Enumerated[CloudExtinction.Preset].all
      sb <- SkyBackground.values
    } yield ObservingConditions(cc, iq, sb, WVAny)

    ocList foreach { oc =>
      val ocString   = "%s,%s,%s".format(oc.iq, oc.cc, oc.sb)
      val searchPath = Conditions.searchPath(oc)
      val pathStr    = searchPath.map(sp => sp.name.get)

      println("%-17s : %s".format(ocString, pathStr.mkString(",")))
    }
  }
}
