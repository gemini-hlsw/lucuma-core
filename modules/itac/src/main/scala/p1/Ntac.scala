// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.p1

import cats.data.NonEmptyList
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Allocation
import lucuma.core.util.TimeSpan


@deprecated
type Ntac = NonEmptyList[Allocation]

object Ntac:
  @deprecated
  def apply(timeAccountingCategory: TimeAccountingCategory, awardedTime: TimeSpan): Ntac =
    NonEmptyList.one(Allocation(timeAccountingCategory, ScienceBand.Band1, awardedTime))

