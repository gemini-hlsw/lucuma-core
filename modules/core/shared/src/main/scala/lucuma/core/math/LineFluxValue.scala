// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.*
import lucuma.core.util.NewType

// Should never be less than zero or larger than 1.
type LineFluxValueRefinement = Interval.Closed[0, 1]
type LineFluxValueType = BigDecimal Refined LineFluxValueRefinement
object LineFluxValueType extends RefinedTypeOps[LineFluxValueType, BigDecimal]

object LineFluxValue extends NewType[LineFluxValueType]:
  def from(t: BigDecimal): Either[String, LineFluxValue] = LineFluxValueType.from(t).map(apply(_))
  def unsafeFrom(x: BigDecimal): LineFluxValue = apply(LineFluxValueType.unsafeFrom(x))
type LineFluxValue = LineFluxValue.Type
