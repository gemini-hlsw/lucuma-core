// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type of seeing trends.
 */
enum SeeingTrend(val tag: String, val name: String) derives Enumerated:
  case GettingBetter  extends SeeingTrend("getting_better",   "Getting Better")
  case GettingWorse   extends SeeingTrend("getting_worse",    "Getting Worse")
  case StayingTheSame extends SeeingTrend("staying_the_same", "Staying the Same")
  case Variable       extends SeeingTrend("variable",         "Variable")
