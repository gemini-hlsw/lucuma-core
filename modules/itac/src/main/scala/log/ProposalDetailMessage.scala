// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.log

/**
 * A trait for all proposal messages.  It includes what is intended to
 * be a brief "reason" message and a longer "detail" message.
 */
trait ProposalDetailMessage extends LogMessage {
  def reason: String
  def detail: String
}