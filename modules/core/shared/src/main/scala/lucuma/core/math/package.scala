// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import eu.timepit.refined.api.*
import eu.timepit.refined.numeric.*
import lucuma.core.util.NewType

/** Mathematical data types for general use, not specific to the Gem model. */
type RA = RightAscension
val RA: RightAscension.type = RightAscension

type Dec = Declination
val Dec: Declination.type = Declination

type Lat = Declination
val Lat: Declination.type = Declination

type Lon = Angle
val Lon: Angle.type = Angle

type BrightnessValueRefinement = Interval.Closed[-100, 100]
type BrightnessValueType = BigDecimal Refined BrightnessValueRefinement
object BrightnessValueType extends RefinedTypeOps[BrightnessValueType, BigDecimal]

object BrightnessValue extends NewType[BrightnessValueType]:
  def from(t: BigDecimal): Either[String, BrightnessValue] = BrightnessValueType.from(t).map(apply(_))
  def unsafeFrom(x: BigDecimal): BrightnessValue = apply(BrightnessValueType.unsafeFrom(x))
type BrightnessValue = BrightnessValue.Type