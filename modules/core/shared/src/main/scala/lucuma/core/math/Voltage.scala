// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb.Quantity
import coulomb.syntax.*
import coulomb.units.mksa.Volt
import lucuma.core.math.units.Microvolt
import lucuma.core.math.units.Millivolt

import scala.util.Try


opaque type Voltage = Quantity[Long, Microvolt]

object Voltage {
  def fromMicrovolts(uv: Long): Voltage = uv.withUnit[Microvolt]
  
  def fromMillivolts(mv: BigDecimal): Option[Voltage] = Try(mv.bigDecimal.movePointRight(3).setScale(0, java.math.RoundingMode.HALF_UP).longValueExact).toOption.map(fromMicrovolts)
  
  def unsafeFromMillivolts(mv: BigDecimal): Voltage = fromMillivolts(mv).getOrElse(sys.error(s"The voltage value ($mv) is out of range."))
  
  def unsafeFromLongMillivolts(mv: Long): Voltage = fromMillivolts(BigDecimal(mv)).getOrElse(sys.error(s"The voltage value ($mv) is out of range."))
  
  def fromVolts(v: BigDecimal): Option[Voltage] = Try(v.bigDecimal.movePointRight(6).setScale(0, java.math.RoundingMode.HALF_UP).longValueExact).toOption.map(fromMicrovolts)

  def unsafeFromVolts(mv: BigDecimal): Voltage = fromVolts(mv).getOrElse(sys.error(s"The voltage value ($mv) is out of range."))


  extension (v: Voltage) {
    private def to[U](scale: Int): Quantity[BigDecimal, U] =
      BigDecimal(v.value, scale).withUnit[U]

    def toMicrovolts: Quantity[Long, Microvolt] = v
    def toMillivolts: Quantity[BigDecimal, Millivolt] = to[Millivolt](3)
    def toVolts: Quantity[BigDecimal, Volt] = to[Volt](6)
  }

}
