// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

/**
 * Observing Mode.
 */
sealed trait Mode extends Ordered[Mode] {
  /** The part of a program id that identifies it as a member of this mode. */
  def programId: String

  /** Whether the queue engine should schedule proposals of this mode. */
  def schedule: Boolean

  def compare(that: Mode): Int = {
    val res = programId.compare(that.programId)
    if (res == 0) schedule.compare(that.schedule)
    else res
  }
}

object Mode {
  case object Classical extends Mode {
    val programId = "C"
    val schedule  = false
  }

  case object LargeProgram extends Mode {
    val programId = "LP"
    val schedule  = true
  }

  case object Queue extends Mode {
    val programId = "Q"
    val schedule  = true
  }

  val All = List(Classical, LargeProgram, Queue)

  def parse(s: String): Option[Mode] = All.find(_.programId == s)
}