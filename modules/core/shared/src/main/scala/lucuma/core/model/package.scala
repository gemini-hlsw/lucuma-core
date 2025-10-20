// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import eu.timepit.refined.internal.WitnessAs
import eu.timepit.refined.numeric.GreaterEqual
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.numeric.Less
import eu.timepit.refined.numeric.NonNegative
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Lat
import lucuma.core.optics.Format
import lucuma.core.refined.given
import lucuma.core.util.NewRefined
import org.typelevel.cats.time.instances.duration.*

import java.time.Duration
import java.time.Instant
import java.time.temporal.Temporal
import scala.math.Pi
import scala.math.abs
import scala.math.pow
import scala.math.sin

type TrackingAt = Instant => Option[Coordinates]

// Integer Percents
type ZeroTo100  = Interval.Closed[0, 100]
type IntPercent = Int Refined ZeroTo100

object IntPercent extends RefinedTypeOps[IntPercent, Int]

// Store a percentage with two decimals of precision for a 0-100% range
type CentiPercent    = Interval.Closed[0, 10000]
object IntCentiPercent extends NewRefined[Int, CentiPercent]:
  val Max = unsafeFrom(10000)
  val Min = unsafeFrom(0)

  def fromPercent(p: BigDecimal): Either[String, IntCentiPercent] =
    from((p * 100).toInt)
  def unsafeFromPercent(p: BigDecimal): IntCentiPercent =
    unsafeFrom((p * 100).toInt)

  val FromBigDecimal: Format[BigDecimal, IntCentiPercent] =
    Format.apply(d => fromPercent(d).toOption, _.toPercent)

  extension(a: IntCentiPercent)
    def toPercent: BigDecimal = a.value.value / 100.0
    def *(b: IntCentiPercent): IntCentiPercent =
      // Given a and b are in the range 0-1 the result is also in the range 0-1
      IntCentiPercent.unsafeFrom((a.value.value * b.value.value) / 10000)
    def round: IntCentiPercent = IntCentiPercent.unsafeFrom(100 * scala.math.round(a.value.value / 100.0f))

type IntCentiPercent = IntCentiPercent.Type

object AirMass extends NewRefined[BigDecimal, Not[Less[1]]]:
  /**
    * Minimum airmass that a given declination reaches from a given latitude.
    */
  def minimumFor(dec: Declination, latitude: Lat): Option[AirMass] =
    // Maximum elevation in degrees
    val elevation = 90.0 - abs(dec.toAngle.toSignedDoubleDegrees - latitude.toAngle.toSignedDoubleDegrees)
    if elevation < 0 then None
    else Some(AirMass.unsafeFrom(BigDecimal(1.0 / sin((elevation + 244.0 / (165.0 + 47.0 * pow(elevation, 1.1))) * Pi / 180.0))))

type AirMass = AirMass.Type

object AirMassBound extends NewRefined[AirMass, Interval.Closed[1, 3]]:
  def fromBigDecimal(b: BigDecimal): Either[String, AirMassBound] =
    AirMass.from(b).flatMap(from(_))
  def unsafeFromBigDecimal(b: BigDecimal): AirMassBound =
    unsafeFrom(AirMass.unsafeFrom(b))

  extension (a: AirMassBound)
    def toBigDecimal: BigDecimal = a.value.value.value.value

  val Min: AirMassBound = unsafeFromBigDecimal(BigDecimal(1))
  val Max: AirMassBound = unsafeFromBigDecimal(BigDecimal(3))
type AirMassBound = AirMassBound.Type

object HourAngleBound extends NewRefined[BigDecimal, Interval.Closed[-5, 5]]:
  extension (h: HourAngleBound)
    def toBigDecimal: BigDecimal = h.value.value

  lazy val Min: HourAngleBound = unsafeFrom(-5)
  lazy val Max: HourAngleBound = unsafeFrom(5)
type HourAngleBound = HourAngleBound.Type

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

