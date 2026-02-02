// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p2.rollover

import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt

import java.time.Instant

/**
 * A collection of rollover observations whose time values should be deducted
 * from the corresponding bins.
 */
case class RolloverReport(
  site:      Site,
  semester:  Semester,
  timestamp: Instant,
  obsList:   List[RolloverObservation]
) {

  /**
   * Filters the report for a particular site.
   */
  def filter(site: Site): RolloverReport =
    copy(obsList = obsList.filter(_.site == site))

  /**
   * Total of all rollover observation times.  This amount of time is subtracted
   * from the available queue time.
   */
  def total: Time = obsList.foldLeft(Time.ZeroHours)(_ + _.time)

}

object RolloverReport {

  /**
   * Parse a `RolloverReport` from an XML element, as provided by the OCS SPDB rollover servlet.
   * Such an element looks like this. Timestamp is a Unix epoch time in milliseconds. The format
   * of <obs> elements is given in RolloverObservation.
   *
   *  <rollover site="GS" semester="2020A" timestamp="1580514160465">
   *    <obs>...</obs>
   *    <obs>...</obs>
   *    ...
   *  </rollover>
   *
   * You can fetch such a report yourself via
   *
   *     curl -i http://gsodb.gemini.edu:8442/rollover
   *
   * @return a RolloverReport, or a message on failure.
   */
  // def fromXml(rollover: Node): Either[String, RolloverReport] =
  //   Try {
  //     val site = Site.parse(rollover \@ "site")
  //     val sem  = Semester.parse(rollover \@ "semester")
  //     val ins  = Instant.ofEpochMilli((rollover \@ "timestamp").toLong)
  //     val ros  = (rollover \ "obs").toList.traverseU(RolloverObservation.fromXml)
  //     ros match {
  //       case Right(ros) => RolloverReport(site, sem, ins, ros)
  //       case Left(e)    => sys.error(e)
  //     }
  //   } .toEither.leftMap(_.getMessage)

  /** An empty report with arbitrary site, semester, timestamp, and no observations. */
  def empty: RolloverReport =
    RolloverReport(Site.GN, Semester(YearInt.unsafeFrom(2020), Half.A), Instant.EPOCH, Nil)

}
