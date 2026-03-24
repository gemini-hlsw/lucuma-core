// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine

import edu.gemini.tac.qengine.util.Time
import lucuma.core.enums.ScienceBand

import p1.{ItacObservation, Proposal}

package object log {
  def rejectConditions(p: Proposal, o: ItacObservation, b: ScienceBand, c: Time, m: Time) = RejectConditions(p, o, b, c, m)
  def rejectTarget(p: Proposal, o: ItacObservation, b: ScienceBand, t: RejectTarget.RaDecType, c: Time, m: Time) = RejectTarget(p, o, b, t, c, m)
}
