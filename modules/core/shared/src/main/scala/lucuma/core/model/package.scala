// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Monoid
import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.api.Validate
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.numeric.GreaterEqual
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.numeric.Less
import eu.timepit.refined.numeric.NonNegative
import lucuma.core.math.units.given
import lucuma.core.optics.Format
import lucuma.core.util.NewRefinedQuantity
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration
import java.time.temporal.Temporal

// Store a percentage with two decimals of precision for a 0-100% range
type CentiPercentRange = Interval.Closed[0, 10000]
type CentiPercent      = Centi * Percent
object Percentile extends NewRefinedQuantity[Int, CentiPercentRange, CentiPercent]:
  def fromPercent(p: Double): Either[String, Percentile] =
    from((p * 100).round.toInt)

  def unsafeFromPercent(p: Double): Percentile =
    unsafeFrom((p * 100).round.toInt)

  val Max: Percentile = unsafeFromPercent(100)
  val Min: Percentile = unsafeFromPercent(0)

  val FromBigDecimal: Format[BigDecimal, Percentile] =
    Format.apply(d => fromPercent(d.toDouble).toOption, _.value.toValue[Int].toValue[BigDecimal].toUnit[Percent].value)

  extension(a: Percentile)
    def toPercent: Double = FromBigDecimal.reverseGet(a).toDouble
    def *(b: Percentile): Percentile = // Given a and b are in the range 0-1 the result is also in the range 0-1
      FromBigDecimal.getOption(FromBigDecimal.reverseGet(a) * FromBigDecimal.reverseGet(b)).get
    def rounded: Percentile = FromBigDecimal.getOption(FromBigDecimal.reverseGet(a).rounded).get
type Percentile = Percentile.Type

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

