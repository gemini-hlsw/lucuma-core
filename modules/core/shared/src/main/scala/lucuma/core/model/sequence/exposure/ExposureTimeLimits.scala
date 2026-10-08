// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.util.TimeSpan

/** An inclusive range of exposure times, open at one end or neither. */
sealed trait ExposureTimeLimits derives Eq:

  def check(time: TimeSpan): ExposureTimeLimits.Result

  def isValid(time: TimeSpan): Boolean =
    check(time) match
      case ExposureTimeLimits.Result.InRange => true
      case _                                 => false

object ExposureTimeLimits:

  /** The outcome of a check, with the limit broken if any. */
  enum Result:
    case InRange
    case Below(min: TimeSpan)
    case Above(max: TimeSpan)

  object Result:
    given Eq[Result] = Eq.fromUniversalEquals

  /** At least `min`. */
  final case class Min(min: TimeSpan) extends ExposureTimeLimits:
    override def check(time: TimeSpan): Result =
      if time < min then Result.Below(min) else Result.InRange

  /** At most `max`. */
  final case class Max(max: TimeSpan) extends ExposureTimeLimits:
    override def check(time: TimeSpan): Result =
      if time > max then Result.Above(max) else Result.InRange

  /** From `min` to `max`, created with `fromMinMax` so that `min <= max`. */
  final case class MinMax private[ExposureTimeLimits] (min: TimeSpan, max: TimeSpan) extends ExposureTimeLimits:
    override def check(time: TimeSpan): Result =
      if time < min then Result.Below(min)
      else if time > max then Result.Above(max)
      else Result.InRange

  def fromMin(min: TimeSpan): ExposureTimeLimits =
    Min(min)

  def fromMax(max: TimeSpan): ExposureTimeLimits =
    Max(max)

  /** Limits from `min` to `max`, if `min <= max`. */
  def fromMinMax(min: TimeSpan, max: TimeSpan): Option[ExposureTimeLimits] =
    Option.when(min <= max)(MinMax(min, max))

  /** Limits from `min` to `max`, which must not be less than `min`. */
  def unsafeFromMinMax(min: TimeSpan, max: TimeSpan): ExposureTimeLimits =
    fromMinMax(min, max).getOrElse:
      throw new IllegalArgumentException(s"Exposure time minimum $min exceeds maximum $max")
