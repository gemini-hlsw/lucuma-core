// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

final case class RightAscensionJS(ra: RightAscension):

  val ha  = ra.toHourAngle
  val µas = ra.toAngle.toMicroarcseconds
  val µs  = ha.toMicroseconds

  def divMicro(micro: Long, denom: Int) = BigDecimal(micro, 6) / denom

  @JSExport val hms: String             = HourAngle.HMS(ha).format
  @JSExport val hours: Double           = divMicro(µs, 3_600).floatValue
  @JSExport val degrees: Double         = divMicro(µas, 3_600).floatValue
  @JSExport val microseconds: js.BigInt = js.BigInt(µs.toString())
