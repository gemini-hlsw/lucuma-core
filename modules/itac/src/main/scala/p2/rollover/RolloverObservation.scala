// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p2.rollover

import edu.gemini.tac.qengine.p1.CategorizedTime
import edu.gemini.tac.qengine.p1.ObservingConditions
import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Site

/**
 * A class that represents a rollover time observation.  The time for each
 * rollover observation should be subtracted from the corresponding bins.
 */
case class RolloverObservation(
  obsId: String, // we don't care as long as it starts with G?-...
  target: Target,
  conditions: ObservingConditions,
  time: Time
) extends CategorizedTime {
  def site: Site = ??? //Site.parse(obsId.take(2))
}

object RolloverObservation {

  /**
   * Parse a `RolloverObservation` from an XML element, as provided by the OCS SPDB rollover
   * servlet. Such an element looks like this. Remaining time is in milliseconds.
   *
   *  <obs>
   *    <id>GS-2019B-Q-107-5</id>
   *    <partner>Argentina</partner>
   *    <target>
   *      <ra>122.252373 deg</ra>
   *      <dec>-61.302384 deg</dec>
   *    </target>
   *    <conditions>
   *      <cc>50</cc>
   *      <iq>70</iq>
   *      <sb>100</sb>
   *      <wv>100</wv>
   *    </conditions>
   *    <time>5497000</time>
   *  </obs
   *
   * You can fetch such a report yourself via
   *
   *     curl -i http://gsodb.gemini.edu:8442/rollover
   *
   * @return a RolloverObservation, or a message on failure.
   */
  // def fromXml(o: scala.xml.Node): Either[String, RolloverObservation] =
  //   Try {

  //     def fail(field: String): Nothing =
  //       sys.error(s"Error parsing RolloverObservation: missing or invalid $field\n$o")

  //     val id   = (o \ "id").text
  //     val time = Time.millisecs((o \ "time").text.toLong).toMinutes
  //     val ra   = new Angle((o \ "target" \ "ra" ).text.takeWhile(_ != ' ').toDouble, Angle.Deg)
  //     val dec  = new Angle((o \ "target" \ "dec").text.takeWhile(_ != ' ').toDouble, Angle.Deg)
  //     val t    = Target(ra, dec, None)
  //     val cc   = CloudCover   .values.find(_.percent == (o \ "conditions" \ "cc").text.toInt).getOrElse(fail("cc"))
  //     val iq   = ImageQuality .values.find(_.percent == (o \ "conditions" \ "iq").text.toInt).getOrElse(fail("iq"))
  //     val sb   = SkyBackground.values.find(_.percent == (o \ "conditions" \ "sb").text.toInt).getOrElse(fail("sb"))
  //     val wv   = WaterVapor   .values.find(_.percent == (o \ "conditions" \ "wv").text.toInt).getOrElse(fail("wv"))

  //     // Almost done!
  //     val ro   = RolloverObservation(id, t, ObservingConditions(cc, iq, sb, wv), time)

  //     // Hack: ensure the id is valid enough to discern the site
  //     try { ro.site } catch { case _: ParseException => fail("observation id")}

  //     // Ok, done.
  //     ro

  //   } .toEither.leftMap(_.getMessage)

}