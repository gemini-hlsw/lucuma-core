// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.impl.block

import edu.gemini.tac.qengine.p1.{Observation, Proposal}
import edu.gemini.tac.qengine.util.Time

/**
 * A block represents a portion of time for a particular observation in a
 * particular program.  The block time may include the entire observation or
 * it may be a portion of it.
 *
 * <p>"Start" blocks are the first in a sequence for a proposal. Blocks are
 * "final" if they contain, relative to other blocks for the same proposal in
 * a sequence, the last amount of time for the proposal.  A short proposal
 * with a single observation may contain only a single block, in which case it
 * would be both the start and final block for the proposal.
 */
final case class Block(prop: Proposal, obs: Observation, time: Time, isStart: Boolean, isFinal: Boolean) {
  def toFinal: Block = Block(prop, obs, time, isStart, isFinal = true)
  def updated(t: Time): Block = Block(prop, obs, t, isStart, isFinal)

  override def toString: String = "Block(%s(%s), %s, %s, %s, %s)".format(
    prop.ntac.partner.id,
    prop.ntac.reference,
    obs,
    time,
    if (isStart) "start" else "not start",
    if (isFinal) "final" else "not final"
  )
}

object Block {
  /**
   * An apply method that is useful for testing, since the majority of the
   * test cases don't care whether the block is a start block or a final
   * block.
   */
  def apply(prop: Proposal, obs: Observation, time: Time): Block =
    new Block(prop, obs, time, false, false)
}

