// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.api.config.ConditionsCategory as Cat
import edu.gemini.tac.qengine.p1.CloudCover
import edu.gemini.tac.qengine.p1.CloudCover.*
import edu.gemini.tac.qengine.p1.ImageQuality
import edu.gemini.tac.qengine.p1.ImageQuality.*
import edu.gemini.tac.qengine.p1.ObservingConditions
import edu.gemini.tac.qengine.p1.SkyBackground
import edu.gemini.tac.qengine.p1.SkyBackground.*
import edu.gemini.tac.qengine.p1.WaterVapor.*
import edu.gemini.tac.qengine.util.Percent
import edu.gemini.tac.qengine.util.Time

import Cat.*

object Default {

  val Conditions = ConditionsCategoryMap.ofPercent(
    (Cat(Eq(CC50), Eq(IQ20), Le(SB50), UnspecifiedWV, Some("1")), 4),
    (Cat(Eq(CC50), Eq(IQ20), Ge(SB80), UnspecifiedWV, Some("2")), 4),
    (Cat(Ge(CC70), Eq(IQ20), UnspecifiedSB, UnspecifiedWV, Some("3")), 3),
    (Cat(Eq(CC50), Eq(IQ70), Le(SB50), UnspecifiedWV, Some("4")), 10),
    (Cat(Eq(CC50), Eq(IQ70), Ge(SB80), UnspecifiedWV, Some("5")), 10),
    (Cat(Ge(CC70), Eq(IQ70), UnspecifiedSB, UnspecifiedWV, Some("6")), 10),
    (Cat(Eq(CC50), Eq(IQ85), UnspecifiedSB, UnspecifiedWV, Some("7")), 15),
    (Cat(Ge(CC70), Eq(IQ85), UnspecifiedSB, UnspecifiedWV, Some("8")), 20),
    (Cat(UnspecifiedCC, Eq(IQAny), UnspecifiedSB, UnspecifiedWV, Some("9")), 40)
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
      iq <- ImageQuality.values
      cc <- CloudCover.values
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
