// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.Angle
import lucuma.core.math.HourAngle

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

final case class AngleJS(a: Angle):

  val ha  = Angle.hourAngle.get(a)
  val µas = a.toMicroarcseconds
  val µs  = ha.toMicroseconds

  def divMicro(micro:    Long, denom: Int) = (BigDecimal(micro, 6) / denom).doubleValue
  def divArcseconds(div: Int)              = divMicro(µas, div)
  def divSeconds(div:    Int)              = divMicro(µs, div)

  @JSExport val arcminutes: Double         = divArcseconds(60)
  @JSExport val arcseconds: Double         = BigDecimal(µas, 6).doubleValue
  @JSExport val degrees: Double            = divArcseconds(3_600)
  @JSExport val dms: String                = Angle.dms.get(a).format.dropRight(3)
  @JSExport val hms: String                = HourAngle.HMS(ha).format.dropRight(3)
  @JSExport val hours: Double              = divSeconds(3_600)
  @JSExport val microarcseconds: js.BigInt = js.BigInt(µas.toString())
  @JSExport val microseconds: Double       = µs.toDouble
  @JSExport val milliarcseconds: Double    = BigDecimal(µas, 3).doubleValue
  @JSExport val milliseconds: Double       = BigDecimal(µs, 3).doubleValue
  @JSExport val minutes: Double            = divSeconds(60)
  @JSExport val seconds: Double            = BigDecimal(µs, 6).doubleValue
