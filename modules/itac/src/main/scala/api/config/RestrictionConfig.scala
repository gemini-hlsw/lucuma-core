// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import lucuma.core.model.IntCentiPercent
import lucuma.core.util.TimeSpan

/**
 * Configuration of restricted bins, which may be specified in terms of a
 * percentage of total queue time or an absolute time.
 */
case class RestrictionConfig(
     relativeTimeRestrictions: List[TimeRestriction[IntCentiPercent]] = Default.RelativeTimeRestrictions,
     absoluteTimeRestrictions: List[TimeRestriction[TimeSpan]]    = Default.AbsoluteTimeRestrictions,
     bandRestrictions:         List[BandRestriction]          = Default.BandRestrictions) {

  /**
   * Maps the IntCentiPercent bins and the Time bins to a common type using two
   * supplied mapping functions, combines and returns the result in a single
   * list of restricted bins of the new type.
   */
  def mapTimeRestrictions[A](f: IntCentiPercent => A, g: TimeSpan => A): List[TimeRestriction[A]] =
    relativeTimeRestrictions.map(_.map(f)) ::: absoluteTimeRestrictions.map(_.map(g))
}