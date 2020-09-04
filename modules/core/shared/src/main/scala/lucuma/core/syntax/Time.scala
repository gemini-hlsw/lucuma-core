// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import java.time.{ Duration, Instant, ZonedDateTime }
import java.time.temporal.TemporalAmount

// A bit of syntax to make working with Java Instant and Duration a bit less
// cumbersome / easier to read.

final class InstantOps(val self: Instant) extends AnyVal {

  def +(t: TemporalAmount): Instant =
    self.plus(t)

  def -(t: TemporalAmount): Instant =
    self.minus(t)

}

trait ToInstantOps {
  implicit def ToInstantOps(i: Instant): InstantOps =
    new InstantOps(i)
}

final class ZonedDateTimeOps(val self: ZonedDateTime) extends AnyVal {

  def +(t: TemporalAmount): ZonedDateTime =
    self.plus(t)

  def -(t: TemporalAmount): ZonedDateTime =
    self.minus(t)

}

trait ToZonedDateTimeOps {
  implicit def ToZonedDateTimeOps(zdt: ZonedDateTime): ZonedDateTimeOps =
    new ZonedDateTimeOps(zdt)
}

final class DurationOps(val self: Duration) extends AnyVal {

  def +(that: Duration): Duration =
    self.plus(that)

  def -(that: Duration): Duration =
    self.minus(that)

  def *(m: Long): Duration =
    self.multipliedBy(m)

  def /(d: Long): Duration =
    self.dividedBy(d)
}

trait ToDurationOps {
  implicit def ToDurationOps(d: Duration): DurationOps =
    new DurationOps(d)
}

object time extends ToInstantOps with ToDurationOps with ToZonedDateTimeOps
