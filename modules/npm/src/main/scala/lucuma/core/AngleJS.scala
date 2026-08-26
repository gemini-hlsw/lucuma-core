// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.Angle
import lucuma.core.math.HourAngle

import scala.scalajs.js

final class AngleJS(a: Angle) extends js.Object:

  private val ha  = Angle.hourAngle.get(a)
  private val µas = a.toMicroarcseconds
  private val µs  = ha.toMicroseconds

  private def divMicro(micro:    Long, denom: Int) = (BigDecimal(micro, 6) / denom).doubleValue
  private def divArcseconds(div: Int)              = divMicro(µas, div)
  private def divSeconds(div:    Int)              = divMicro(µs, div)

  val arcminutes: Double         = divArcseconds(60)
  val arcseconds: Double         = BigDecimal(µas, 6).doubleValue
  val degrees: Double            = divArcseconds(3_600)
  val dms: String                = Angle.dms.get(a).format.dropRight(3)
  val hms: String                = HourAngle.HMS(ha).format.dropRight(3)
  val hours: Double              = divSeconds(3_600)
  val microarcseconds: js.BigInt = js.BigInt(µas.toString())
  val microseconds: Double       = µs.toDouble
  val milliarcseconds: Double    = BigDecimal(µas, 3).doubleValue
  val milliseconds: Double       = BigDecimal(µs, 3).doubleValue
  val minutes: Double            = divSeconds(60)
  val seconds: Double            = BigDecimal(µs, 6).doubleValue
