// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.syntax.all.*
import lucuma.core.arb.ArbTime
import lucuma.core.math.BoundedInterval
import lucuma.core.syntax.time.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose
import org.typelevel.cats.time.*
import spire.math.Interval
import spire.math.extras.interval.IntervalSeq
import spire.math.interval.Closed
import spire.math.interval.EmptyBound
import spire.math.interval.Open
import spire.math.interval.Unbound
import spire.math.interval.ValueBound

import java.time.Duration
import java.time.Instant

trait IntervalGens {
  import ArbTime.*

  private val MaxDelta: Long = Duration.ofMinutes(10).toNanos

  def buildInterval(start: Int, end: Int): BoundedInterval[Instant] =
    BoundedInterval.unsafeOpenUpper(Instant.ofEpochMilli(start.toLong), Instant.ofEpochMilli(end.toLong))

  implicit val chooseInstant: Choose[Instant] = new Choose[Instant] {
    def choose(min: Instant, max: Instant): Gen[Instant] =
      for {
        seconds <- Gen.choose(min.getEpochSecond, max.getEpochSecond)
        nanosMin = if (seconds === min.getEpochSecond) min.getNano.toLong else 0L
        nanosMax = if (seconds === max.getEpochSecond) max.getNano.toLong
                   else Constants.NanosInSecond - 1
        nanos   <- Gen.choose(nanosMin, nanosMax)
      } yield Instant.ofEpochSecond(seconds, nanos)
  }

  def instantInInterval(
    interval: Interval[Instant],
    specials: List[Instant] = List.empty
  ): Gen[Instant] = {
    val lowerBound: Option[ValueBound[Instant]] = interval.lowerBound match {
      case Unbound()    => Closed(Instant.MIN).some
      case EmptyBound() => none
      case Open(a)      => Open(a).some
      case Closed(a)    => Closed(a).some
    }
    val upperBound: Option[ValueBound[Instant]] = interval.upperBound match {
      case Unbound()    => Closed(Instant.MAX).some
      case EmptyBound() => none
      case Open(a)      => Open(a).some
      case Closed(a)    => Closed(a).some
    }
    (lowerBound, upperBound)
      .mapN((lower, upper) =>
        instantInBounded(Interval.fromBounds(lower, upper).asInstanceOf[BoundedInterval[Instant]], specials)
      )
      .getOrElse(Gen.fail)
  }

  def instantInBounded(
    interval: BoundedInterval[Instant],
    specials: List[Instant] = List.empty
  ): Gen[Instant] = {
    val basics            = List(Instant.MIN, Instant.MAX)
    val basicsAndSpecials =
      (basics ++ specials)
        .filter(i => i > interval.lower || (i === interval.lower && interval.lowerBound.isClosed))
        .filter(i => i < interval.upper || (i === interval.upper && interval.upperBound.isClosed))
    val freqs             =
      basicsAndSpecials.map(v => (1, Gen.const(v))) :+ (
        (
          15 + basicsAndSpecials.length,
          Gen.choose(interval.lower, interval.upper)
        )
      )

    Gen
      .frequency(freqs: _*)
      .suchThat(_ > interval.lower || interval.lowerBound.isClosed)
      .suchThat(_ < interval.upper || interval.upperBound.isClosed)
  }

  // There might not be instants outside the interval if the interval is (Instant.MIN, Instant.MAX).
  def instantBeforeInterval(interval: BoundedInterval[Instant]): Gen[Option[Instant]] =
    interval.lowerBound match {
      case Closed(Instant.MIN) => Gen.const(none)
      case Closed(a)           => instantInInterval(Interval.below(a)).map(_.some)
      case Open(a)             => instantInInterval(Interval.atOrBelow(a)).map(_.some)
    }

  def instantAfterInterval(interval: BoundedInterval[Instant]): Gen[Option[Instant]] =
    interval.upperBound match {
      case Closed(Instant.MAX) => Gen.const(none)
      case Closed(a)           => instantInInterval(Interval.above(a)).map(_.some)
      case Open(a)             => instantInInterval(Interval.atOrAbove(a)).map(_.some)
    }

  def instantOutsideInterval(interval: BoundedInterval[Instant]): Gen[Option[Instant]] =
    Gen.oneOf(instantBeforeInterval(interval), instantAfterInterval(interval))

  def instantWithSpecialInterval(interval: BoundedInterval[Instant]): Gen[Instant] =
    Gen.frequency(
      (1, Gen.const(interval.lower)),
      (1, Gen.const(interval.upper)),
      (18, arbitrary[Instant])
    )

  def instantUntilEndOfInterval(interval: BoundedInterval[Instant]): Gen[Instant] =
    instantInInterval(
      Interval.fromBounds(Unbound(), interval.upperBound),
      specials = List(interval.lower, interval.upper)
    )

  def instantFromStartOfInterval(interval: BoundedInterval[Instant]): Gen[Instant] =
    instantInInterval(
      Interval.fromBounds(interval.lowerBound, Unbound()),
      specials = List(interval.lower, interval.upper)
    )

  def rateForInterval(interval: BoundedInterval[Instant]): Gen[Duration] =
    for {
      samples <- Gen.choose(50L, 400L)
      delta   <- Gen.choose(0L, MaxDelta)
    } yield (interval.duration / samples).plusNanos(delta)

  def distinctZip[A: Eq](gen1: Gen[A], gen2: Gen[A]): Gen[(A, A)] =
    Gen.zip(gen1, gen2).suchThat(t => t._1 =!= t._2)

  def distinctZipOpt[A: Eq](gen1: Gen[Option[A]], gen2: Gen[Option[A]]): Gen[Option[(A, A)]] =
    Gen.zip(gen1, gen2).map(_.tupled).suchThat(_.forall(t => t._1 =!= t._2))

  // Schedule may be empty
  def instantInSchedule(schedule: IntervalSeq[Instant]): Gen[Option[Instant]] =
    if (schedule.isEmpty)
      Gen.const(none)
    else
      Gen
        .oneOf(schedule.intervals.toList)
        .flatMap(i => instantInInterval(i).map(_.some))

  def instantOutsideSchedule(schedule: IntervalSeq[Instant]): Gen[Option[Instant]] =
    instantInSchedule(~schedule)

  def intervalInSchedule(schedule: IntervalSeq[Instant]): Gen[Option[BoundedInterval[Instant]]] =
    if (schedule.isEmpty)
      Gen.const(none)
    else
      Gen
        .oneOf(schedule.intervals)
        .flatMap(i =>
          distinctZip(instantInInterval(i), instantInInterval(i))
            .map(BoundedInterval.openUpperFromTuple[Instant].getOption)
        )

  // We define a "section" as a maximal interval either outside or inside the Schedule.
  def sectionInSchedule(schedule: IntervalSeq[Instant]): Gen[BoundedInterval[Instant]] =
    Gen.oneOf(
      intervalInSchedule(schedule).flatMap(_.map(Gen.const).getOrElse(Gen.fail)),
      intervalInSchedule(~schedule).flatMap(_.map(Gen.const).getOrElse(Gen.fail))
    )
}
