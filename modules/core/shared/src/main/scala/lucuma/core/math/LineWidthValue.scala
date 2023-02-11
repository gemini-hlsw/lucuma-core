// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.*
import lucuma.core.util.NewType

// The line width must be positive. For upper limit, we could probably safely use 1e6 km/s.
type LineWidthValueRefinement = Interval.OpenClosed[0, 1_000_000]
type LineWidthValueType = BigDecimal Refined LineWidthValueRefinement
object LineWidthValueType extends RefinedTypeOps[LineWidthValueType, BigDecimal]

object LineWidthValue extends NewType[LineWidthValueType]:
  def from(t: BigDecimal): Either[String, LineWidthValue] = LineWidthValueType.from(t).map(apply(_))
  def unsafeFrom(x: BigDecimal): LineWidthValue = apply(LineWidthValueType.unsafeFrom(x))
type LineWidthValue = LineWidthValue.Type
