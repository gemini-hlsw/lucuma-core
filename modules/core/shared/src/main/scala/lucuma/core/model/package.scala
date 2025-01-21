// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Monoid
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.api.Validate
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric.GreaterEqual
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.numeric.Less
import eu.timepit.refined.numeric.NonNegative
import lucuma.core.optics.Format
import lucuma.core.util.RefinedNewType
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration
import java.time.temporal.Temporal

// Integer Percents
type ZeroTo100  = Interval.Closed[0, 100]
type IntPercent = Int Refined ZeroTo100

object IntPercent extends RefinedTypeOps[IntPercent, Int]

// Store a percentage with two decimals of precision for a 0-100% range
type CentiPercent    = Interval.Closed[0, 10000]
object IntCentiPercent extends RefinedNewType[Int, CentiPercent]:
  val Max = IntCentiPercent.unsafeFrom(10000)
  val Min = IntCentiPercent.unsafeFrom(0)

  val fromBigDecimal: Format[BigDecimal, IntCentiPercent] =
    Format.apply(d => IntCentiPercent.from((d * 100).toInt).toOption, _.value.value / 100.0)

  extension(a: IntCentiPercent)
    def toPercent: Double = a.value.value / 100.0
    def *(b: IntCentiPercent): IntCentiPercent =
      // Given a and b are in the range 0-1 the result is also in the range 0-1
      IntCentiPercent.unsafeFrom((a.value.value * b.value.value) / 10000)
    def round: IntCentiPercent = IntCentiPercent.unsafeFrom(100 * scala.math.round(a.value.value / 100.0f))

type IntCentiPercent = IntCentiPercent.Type

type AirMassPredicate = Not[Less[1]]
type AirMassValue     = BigDecimal Refined AirMassPredicate

// Non negative duration
given Plain[Duration, GreaterEqual[0]] =
  Validate.fromPredicate(
    (d: Duration) => d.toNanos >= 0L,
    (d: Duration) => s"$d is not negative",
    Not(Less(0))
  )

type NonNegDuration = Duration Refined NonNegative

object NonNegDuration extends RefinedTypeOps[NonNegDuration, Duration]:

  val zero: NonNegDuration =
    unsafeFrom(Duration.ofNanos(0L))

  def between(startInclusive: Temporal, endExclusive: Temporal): NonNegDuration =
    NonNegDuration
      .unapply(Duration.between(startInclusive, endExclusive))
      .getOrElse(zero)

  given Eq[NonNegDuration] =
    Eq.instance { case (a, b) => a.value === b.value }

  given Monoid[NonNegDuration] =
    Monoid.instance(
      NonNegDuration.zero,
      (a, b) => NonNegDuration.unsafeFrom(a.value |+| b.value)
    )

