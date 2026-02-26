// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util

import cats.Monoid
import cats.Order
import lucuma.core.model.IntCentiPercent
import Time.Units

object Time {
  
  sealed abstract class Units extends Ordered[Units] with Serializable {
    def msInUnit: Long
    def toTime(time: java.math.BigDecimal) : Time = toTime(time.doubleValue())
    def toTime(time: Double): Time = new Time(toMs(time), this)
    def toMs(time: Double): Long = (time * msInUnit).round
    def toUnits(ms: Long): Double = ms / msInUnit.toDouble
    def compare(that: Units): Int = msInUnit match {
      case n if n < that.msInUnit => -1
      case n if n > that.msInUnit =>  1
      case _ => 0
    }
    def zero: Time
  }

  object Millisecs extends Units {
    def msInUnit =  1L                       // 1 (Long)
    override def toString = "ms"
    override def zero = ZeroMillisecs
  }

  object Seconds extends Units {
    def msInUnit =  1000L                   // 1000 ms in a sec
    override def toString = "sec"
    override def zero = ZeroSeconds
  }

  object Minutes extends Units {
    def msInUnit = 60L * Seconds.msInUnit   // 60 sec in a minute
    override def toString = "min"
    override def zero = ZeroMinutes
  }

  object Hours extends Units {
    def msInUnit = 60L * Minutes.msInUnit  // 60 min in an hour
    override def toString = "hrs"
    override def zero = ZeroHours
  }

  val units = List(Millisecs, Seconds, Minutes, Hours)

  val Zero          = new Time(0, Millisecs)
  val ZeroMillisecs = Zero
  val ZeroSeconds   = Zero.toSeconds
  val ZeroMinutes   = Zero.toMinutes
  val ZeroHours     = Zero.toHours

  def millisecs(ms: Long) = new Time(ms, Millisecs)
  def seconds(secs: Double) = new Time(Seconds.toMs(secs), Seconds)
  def minutes(mins: Double) = new Time(Minutes.toMs(mins), Minutes)
  def hours(hours: Double) = new Time(Hours.toMs(hours), Hours)

  given Monoid[Time] = Monoid.instance(Zero, _ + _)
  given Order[Time]  = Order.by(_.ms)

}

final class Time private (val ms: Long, val unit: Units) {
  assert(ms >= 0)

  def isZero = ms == 0
  def toZero = if (ms == 0) this else new Time(0, unit)

  def value: Double = unit.toUnits(ms)

  def to(unit: Units): Time =
    if (unit == this.unit) this else new Time(ms, unit)

  def toMillisecs: Time = to(Time.Millisecs)
  def toSeconds: Time   = to(Time.Seconds)
  def toMinutes: Time   = to(Time.Minutes)
  def toHours: Time     = to(Time.Hours)

  override def hashCode: Int = ms.hashCode
  override def toString = s"$value $unit"

  override def equals(other: Any): Boolean = 
    other match
      case that: Time => ms == that.ms
      case _ => false


  def +(that: Time): Time = new Time(ms + that.ms, unit)

  def -(that: Time): Time =
    val ms2 = ms - that.ms
    if ms2 >= 0 then Time(ms2, unit) else unit.zero

  def *(p: IntCentiPercent): Time = percent(p.toPercent.toDouble)
  def /(d: Double): Time = new Time((ms / d.toLong), unit)

  def percent(p: Double): Time = p match {
    case 0.0 => toZero
    case 100.0 => this
    case _ => new Time((ms * (p/100.0)).round, unit)
  }

}
