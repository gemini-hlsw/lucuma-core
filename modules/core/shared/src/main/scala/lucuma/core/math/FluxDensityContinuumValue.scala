// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.*
import lucuma.core.util.NewType

// Should never be less than zero or larger than 1.
type FluxDensityContinuumValueRefinement = Interval.Closed[0, 1]
type FluxDensityContinuumValueType = BigDecimal Refined FluxDensityContinuumValueRefinement
object FluxDensityContinuumValueType extends RefinedTypeOps[FluxDensityContinuumValueType, BigDecimal]

object FluxDensityContinuumValue extends NewType[FluxDensityContinuumValueType]:
  def from(t: BigDecimal): Either[String, FluxDensityContinuumValue] = FluxDensityContinuumValueType.from(t).map(apply(_))
  def unsafeFrom(x: BigDecimal): FluxDensityContinuumValue = apply(FluxDensityContinuumValueType.unsafeFrom(x))
type FluxDensityContinuumValue = FluxDensityContinuumValue.Type

