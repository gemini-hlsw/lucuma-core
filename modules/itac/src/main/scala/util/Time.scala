// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util

import cats.Monoid

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

  val HoursPerNight = 10

  object Nights extends Units {
    def msInUnit =  HoursPerNight * Hours.msInUnit  // 10 hours in a night
    override def toString = "nts"
    override def zero = ZeroNights
  }

  val units = List(Millisecs, Seconds, Minutes, Hours, Nights)

  val Zero          = new Time(0, Millisecs)
  val ZeroMillisecs = Zero
  val ZeroSeconds   = Zero.toSeconds
  val ZeroMinutes   = Zero.toMinutes
  val ZeroHours     = Zero.toHours
  val ZeroNights    = Zero.toNights

  def millisecs(ms: Long) = new Time(ms, Millisecs)
  def seconds(secs: Double) = new Time(Seconds.toMs(secs), Seconds)
  def minutes(mins: Double) = new Time(Minutes.toMs(mins), Minutes)
  def hours(hours: Double) = new Time(Hours.toMs(hours), Hours)
  def nights(nights: Double) = new Time(Nights.toMs(nights), Nights)

  def min(t0: Time, t1: Time): Time = if (t0 <= t1) t0 else t1
  def max(t0: Time, t1: Time): Time = if (t0 >= t1) t0 else t1

  // Allow expressions like p * t (and not just t * p)
  class PercentMultiplier(p: Percent) {
    def *(t: Time): Time = t * p
  }
  implicit val toPercentMultiplier: Percent => PercentMultiplier = (p: Percent) => new PercentMultiplier(p)

  implicit val MonoidTime: Monoid[Time] =
    Monoid.instance(Zero, _ + _)
}

final class Time private (val ms: Long, val unit: Units) extends Ordered[Time] with Serializable {

  def isZero = ms == 0
  def toZero = if (ms == 0) this else new Time(0, unit)

  def value: Double = unit.toUnits(ms)
  def compare(that: Time): Int = ms match {
    case n if n < that.ms => -1
    case n if n > that.ms =>  1
    case _                =>  0
  }

  def to(unit: Units): Time =
    if (unit == this.unit) this else new Time(ms, unit)

  def toMillisecs: Time = to(Time.Millisecs)
  def toSeconds: Time   = to(Time.Seconds)
  def toMinutes: Time   = to(Time.Minutes)
  def toHours: Time     = to(Time.Hours)
  def toNights: Time    = to(Time.Nights)

  override def toString = value.toString + " " + unit.toString

  override def equals(other: Any): Boolean = other match {
       case that: Time => ms == that.ms
       case _ => false
     }

  override def hashCode: Int = ms.hashCode

  def +(that: Time): Time = new Time(ms + that.ms, unit)
  def -(that: Time): Time = new Time(ms - that.ms, unit)
  def *(p: Percent): Time = percent(p.value.toDouble)
  def unary_- : Time      = new Time(-ms, unit)

  def percent(p: Double): Time = p match {
      case 0.0 => toZero
      case 100.0 => this
      case _ => new Time((ms * (p/100.0)).round, unit)
    }

  def min(that: Time): Time = if (this < that) this else that
  def max(that: Time): Time = if (this > that) this else that

//  def +(amt: Double): Time = Time(ms + unit.toMs(amt), unit)
//  def *(amt: Double): Time = Time(ms * unit.toMs(amt), unit)
//  def -(amt: Double): Time = Time(ms - unit.toMs(amt), unit)
//  def /(amt: Double): Time = Time((ms.toDouble / unit.toMs(amt).toDouble).round, unit)

}
