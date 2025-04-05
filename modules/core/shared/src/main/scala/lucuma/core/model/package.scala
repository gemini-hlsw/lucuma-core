// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Monoid
import cats.syntax.all.*
import coulomb.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.api.Validate
import eu.timepit.refined.api.Validate.Plain
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.internal.WitnessAs
import eu.timepit.refined.numeric.GreaterEqual
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.numeric.Less
import eu.timepit.refined.numeric.NonNegative
import lucuma.core.math.Declination
import lucuma.core.math.Lat
import lucuma.core.optics.Format
import lucuma.core.refined.given
import lucuma.core.util.NewRefined
import lucuma.core.util.NewRefinedQuantity
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration
import java.time.temporal.Temporal
import scala.math.Pi
import scala.math.abs
import scala.math.pow
import scala.math.sin

// Store a percentage with two decimals of precision for a 0-100% range
type CentiPercentRange = Interval.Closed[0, 10000]
type CentiPercent      = Centi * Percent
object Percentile extends NewRefinedQuantity[Int, CentiPercentRange, CentiPercent]:
  def fromPercent(p: BigDecimal): Either[String, Percentile] =
    from((p * 100).toInt)

  def unsafeFromPercent(p: BigDecimal): Percentile =
    unsafeFrom((p * 100).toInt)

  val Max: Percentile = unsafeFromPercent(100)
  val Min: Percentile = unsafeFromPercent(0)

  val FromBigDecimal: Format[BigDecimal, Percentile] =
    Format.apply(d => fromPercent(d).toOption, _.toPercent)

  extension(a: Percentile)
    def toPercent: BigDecimal = BigDecimal(a.value.value.value / 100.0)
    def *(b: Percentile): Percentile = // Given a and b are in the range 0-1 the result is also in the range 0-1
      FromBigDecimal.getOption(FromBigDecimal.reverseGet(a).toDouble * FromBigDecimal.reverseGet(b) / 100.0).get
    def rounded: Percentile = FromBigDecimal.getOption(FromBigDecimal.reverseGet(a).rounded).get
type Percentile = Percentile.Type

object AirMass extends NewRefined[BigDecimal, Not[Less[1]]]:
  /**
    * Minimum airmass that a given declination reaches from a given latitude.
    */
  def minimumFor(dec: Declination, latitude: Lat): AirMass =
    // Maximum elevation in degrees
    val elevation = 90.0 - abs(dec.toAngle.toSignedDoubleDegrees - latitude.toAngle.toSignedDoubleDegrees)
    AirMass.from(BigDecimal(1.0 / sin((elevation + 244.0 / (165.0 + 47.0 * pow(elevation, 1.1))) * Pi / 180.0))).getOrElse(sys.error("Not possible"))

type AirMass = AirMass.Type

object AirMassConstraint extends NewRefined[AirMass, Interval.Closed[1, 3]]:
  def fromBigDecimal(b: BigDecimal): Either[String, AirMassConstraint] =
    AirMass.from(b).flatMap(from(_))
  def unsafeFromBigDecimal(b: BigDecimal): AirMassConstraint =
    unsafeFrom(AirMass.unsafeFrom(b))

  val Min: AirMassConstraint = unsafeFromBigDecimal(BigDecimal(1))
  val Max: AirMassConstraint = unsafeFromBigDecimal(BigDecimal(3))
type AirMassConstraint = AirMassConstraint.Type


object HourAngleConstraint extends NewRefined[BigDecimal, Interval.Closed[-5, 5]]:
  lazy val Min: HourAngleConstraint = unsafeFrom(-5)
  lazy val Max: HourAngleConstraint = unsafeFrom(5)
type HourAngleConstraint = HourAngleConstraint.Type

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

