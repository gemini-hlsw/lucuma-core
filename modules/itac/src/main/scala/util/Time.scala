// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util

import lucuma.core.util.TimeSpan

type Time = TimeSpan

object Time {
  export TimeSpan.*
  
  def fromMillisecondsBounded(ms: Long): TimeSpan = 
    if ms < 0 then TimeSpan.Min else TimeSpan.fromMilliseconds(ms).getOrElse(TimeSpan.Max)

  def fromSecondsBounded(secs: Double): TimeSpan = 
    if secs < 0 then TimeSpan.Min else TimeSpan.fromSeconds(secs).getOrElse(TimeSpan.Max)

  def fromMinutesBounded(mins: Double): TimeSpan = 
    if mins < 0 then TimeSpan.Min else TimeSpan.fromMinutes(mins).getOrElse(TimeSpan.Max)

  def fromHoursBounded(hours: Double): TimeSpan = 
    if hours < 0 then TimeSpan.Min else TimeSpan.fromHours(hours).getOrElse(TimeSpan.Max)
    
}
