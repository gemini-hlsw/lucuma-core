// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.queue

import cats.syntax.all.*
import edu.gemini.tac.qengine.p1._
import edu.gemini.tac.qengine.api.queue.time.QueueTime
import edu.gemini.tac.qengine.util.{BoundedTime, Time}
import edu.gemini.tac.qengine.ctx.Partner

/** A queue for a single band. */
trait ProposalQueue {

  def band: QueueBand

  def queueTime: QueueTime

  def usedTime: Time =
    toList.foldMap(_.time)

  def usedTime(p: Partner): Time =
    toList.filter(_.ntac.partner === p).foldMap(_.time)

  def remainingTime(partner: Partner): Time =
    queueTime(partner) - usedTime(partner)

  def bounds(p: Partner): BoundedTime =
    BoundedTime(queueTime(p), usedTime(p))

  def toList: List[Proposal]

}