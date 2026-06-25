// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.RightAscension
import lucuma.core.math.validation.MathValidators

import scala.scalajs.js

final class RightAscensionJS(ra: RightAscension) extends js.Object:

  private val ha  = ra.toHourAngle
  private val µas = ra.toAngle.toMicroarcseconds
  private val µs  = ha.toMicroseconds

  private def divMicro(micro: Long, denom: Int) = BigDecimal(micro, 6) / denom

  val hms: String             = MathValidators.truncatedRA.reverseGet(ra)
  val hours: Double           = divMicro(µs, 3_600).floatValue
  val degrees: Double         = divMicro(µas, 3_600).floatValue
  val microseconds: js.BigInt = js.BigInt(µs.toString())
