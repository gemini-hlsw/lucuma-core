// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.*
import lucuma.core.util.NewType

// The sun is -27 magnitudes. Maximum in Janskys is 1e8.
type BrightnessValueRefinement = Interval.Closed[-30, 100_000_000]
type BrightnessValueType = BigDecimal Refined BrightnessValueRefinement
object BrightnessValueType extends RefinedTypeOps[BrightnessValueType, BigDecimal]

object BrightnessValue extends NewType[BrightnessValueType]:
  def from(t: BigDecimal): Either[String, BrightnessValue] = BrightnessValueType.from(t).map(apply(_))
  def unsafeFrom(x: BigDecimal): BrightnessValue = apply(BrightnessValueType.unsafeFrom(x))
type BrightnessValue = BrightnessValue.Type