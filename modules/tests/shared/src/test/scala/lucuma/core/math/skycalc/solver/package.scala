// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc

import cats.implicits._
import cats.Eq
import java.time.Duration
import java.time.Instant
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose
import org.scalacheck.Arbitrary._
import gsp.math.arb.ArbTime._
import io.chrisdavenport.cats.time._

package object solver {
  private val MaxDelta: Long = Duration.ofMinutes(10).toNanos

  def buildInterval(start: Int, end: Int): Interval =
    Interval.unsafe(Instant.ofEpochMilli(start.toLong), Instant.ofEpochMilli(end.toLong))

  implicit val chooseInstant: Choose[Instant] = new Choose[Instant] {
    def choose(min: Instant, max: Instant): Gen[Instant] =
      for {
        seconds <- Gen.choose(min.getEpochSecond, max.getEpochSecond)
        nanosMin = if (seconds === min.getEpochSecond) min.getNano.toLong else 0L
        nanosMax = if (seconds === max.getEpochSecond) max.getNano.toLong
                   else Constants.NanosPerSecond - 1
        nanos   <- Gen.choose(nanosMin, nanosMax)
      } yield Instant.ofEpochSecond(seconds, nanos)
  }

  def instantInInterval(
    interval:     Interval,
    includeStart: Boolean = true,
    includeEnd:   Boolean = false,
    specials:     List[Instant] = List.empty
  ): Gen[Instant] = {
    val basics            = List(Instant.MIN, Instant.MAX)
    val basicsAndSpecials =
      (basics ++ specials)
        .filter(i => i > interval.start || (i === interval.start && includeStart))
        .filter(i => i < interval.end || (i === interval.end && includeEnd))
    val freqs             =
      basicsAndSpecials.map(v => (1, Gen.const(v))) :+ ((15 + basicsAndSpecials.length,
                                                         Gen
                                                           .choose(interval.start, interval.end)
                                                        )
      )

    Gen
      .frequency(freqs: _*)
      .suchThat(_ > interval.start || includeStart)
      .suchThat(_ < interval.end || includeEnd)
  }

  // There might not be instants outside the interval if the interval is (Instant.MIN, Instant.MAX).
  def instantBeforeInterval(
    interval:     Interval,
    includeStart: Boolean = false
  ): Gen[Option[Instant]] =
    Interval(Instant.MIN, interval.start).fold(
      Gen.const(if (includeStart) interval.start.some else none)
    )(before =>
      Gen.some(
        instantInInterval(before, includeEnd = includeStart, specials = List(interval.start))
      )
    )

  def instantAfterInterval(interval: Interval, includeEnd: Boolean = true): Gen[Option[Instant]] =
    Interval(interval.end, Instant.MAX)
      .fold(Gen.const(if (includeEnd) interval.end.some else none))(after =>
        Gen.some(instantInInterval(after, includeStart = includeEnd, specials = List(interval.end)))
      )

  def instantOutsideInterval(
    interval:     Interval,
    includeStart: Boolean = false,
    includeEnd:   Boolean = true
  ): Gen[Option[Instant]] =
    Gen.oneOf(
      instantBeforeInterval(interval, includeStart),
      instantAfterInterval(interval, includeEnd)
    )

  def instantWithSpecialInterval(interval: Interval): Gen[Instant] =
    Gen.frequency((1, Gen.const(interval.start)),
                  (1, Gen.const(interval.end)),
                  (18, arbitrary[Instant])
    )

  def instantUntilEndOfInterval(interval: Interval, includeEnd: Boolean = false): Gen[Instant] =
    instantInInterval(Interval.unsafe(Instant.MIN, interval.end),
                      includeEnd,
                      specials = List(interval.start, interval.end)
    )

  def instantFromStartOfInterval(interval: Interval, includeStart: Boolean = true): Gen[Instant] =
    instantInInterval(Interval.unsafe(interval.start, Instant.MAX),
                      includeStart,
                      specials = List(interval.start, interval.end)
    )

  def rateForInterval(interval: Interval): Gen[Duration] =
    for {
      samples <- Gen.choose(50L, 400L)
      delta   <- Gen.choose(0, MaxDelta)
    } yield interval.duration.dividedBy(samples).plusNanos(delta)

  def distinctZip[A: Eq](gen1: Gen[A], gen2: Gen[A]): Gen[(A, A)] =
    Gen.zip(gen1, gen2).suchThat(t => t._1 =!= t._2)

  def distinctZipOpt[A: Eq](gen1: Gen[Option[A]], gen2: Gen[Option[A]]): Gen[Option[(A, A)]] =
    Gen.zip(gen1, gen2).map(_.tupled).suchThat(_.forall(t => t._1 =!= t._2))

  // Schedule may be empty
  def instantInSchedule(
    schedule:      Schedule,
    includeStarts: Boolean = true,
    includeEnds:   Boolean = false
  ): Gen[Option[Instant]] =
    if (schedule.isEmpty)
      Gen.const(none)
    else
      Gen
        .oneOf(schedule.intervals)
        .flatMap(i => instantInInterval(i, includeStarts, includeEnds).map(_.some))

  def instantOutsideSchedule(
    schedule:      Schedule,
    includeStarts: Boolean = false,
    includeEnds:   Boolean = true
  ): Gen[Option[Instant]] =
    if (schedule.isEmpty)
      arbitrary[Instant].map(_.some)
    else {
      val gaps = schedule.gaps
      Gen.frequency(
        (1, instantBeforeInterval(schedule.intervals.head, includeStart = includeStarts)),
        (1, instantAfterInterval(schedule.intervals.last, includeEnd = includeEnds)),
        (gaps.intervals.length,
         instantInSchedule(gaps, includeStarts = includeEnds, includeEnds = includeStarts)
        )
      )
    }

  def intervalInSchedule(schedule: Schedule): Gen[Option[Interval]] =
    if (schedule.isEmpty)
      Gen.const(none)
    else
      Gen
        .oneOf(schedule.intervals)
        .flatMap(i =>
          distinctZip(instantInInterval(i), instantInInterval(i))
            .map(Interval.fromInstants.getOption)
        )

  // We define a "section" as a maximal interval either outside or inside the Schedule.
  def sectionInSchedule(schedule: Schedule): Gen[Interval] = {
    val breaks = ((Instant.MIN +: schedule.intervals.flatMap(i =>
      List(i.start, i.end)
    )) :+ Instant.MAX).distinct
    Gen.oneOf(breaks.sliding(2).map { case List(s, e) => Interval.unsafe(s, e) }.toList)
  }
}
