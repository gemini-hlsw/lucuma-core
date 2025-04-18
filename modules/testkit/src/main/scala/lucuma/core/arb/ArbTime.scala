// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.arb

import cats.syntax.all.*
import lucuma.core.syntax.time.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.*
import org.typelevel.cats.time.*

import java.time.*
import scala.concurrent.duration.Duration as SDuration
import scala.jdk.CollectionConverters.*

// Arbitrary but reasonable dates and times.
trait ArbTime {

  given Arbitrary[ZoneId] =
    Arbitrary {
      oneOf(ZoneId.getAvailableZoneIds.asScala.toSeq).map(ZoneId.of)
    }

  given Arbitrary[Year] =
    Arbitrary {
      choose(2000, 2020).map(Year.of)
    }

  given Arbitrary[LocalDate] =
    Arbitrary {
      for {
        y <- arbitrary[Year]
        d <- choose(1, y.length)
      } yield LocalDate.ofYearDay(y.getValue, d)
    }

  given Arbitrary[LocalTime] =
    Arbitrary {
      for {
        h <- choose(0, 23)
        m <- choose(0, 59)
        s <- choose(0, 59)
        n <- choose(0, 999999999)
      } yield LocalTime.of(h, m, s, n)
    }

  given Arbitrary[LocalDateTime] =
    Arbitrary {
      for {
        d <- arbitrary[LocalDate]
        t <- arbitrary[LocalTime]
      } yield LocalDateTime.of(d, t)
    }

  // Special case for instants in transitions
  // We can't cache this in a TrieMap since it's not available in Scala.js,
  // and it doesn't seem worthwhile dealing with Java's ConcurrentMap for this.
  private def transitions(zid: ZoneId): List[Instant] = {
    val span  = Period.ofYears(50)
    val now   = Instant.now.atZone(zid)
    val start = (now - span).toInstant
    val end   = (now + span).toInstant
    List.unfold(start)(i =>
      Option(zid.getRules.nextTransition(i)).flatMap(transition =>
        transition.getInstant.some
          .filter(_ < end)
          .map(instant => (instant, instant))
      )
    )
  }

  def genArbitraryZDT(zid: ZoneId): Gen[ZonedDateTime] =
    arbitrary[LocalDateTime].map(ldt => ZonedDateTime.of(ldt, zid))

  def genTransitioningZDT(zid: ZoneId): Gen[ZonedDateTime] =
    transitions(zid) match {
      case Nil  => genArbitraryZDT(zid)
      case list => Gen.oneOf(list).map(_.atZone(zid))
    }

  val genZonedDateTime: Gen[ZonedDateTime] =
    arbitrary[ZoneId].flatMap(zid =>
      Gen.frequency((1, genTransitioningZDT(zid)), (9, genArbitraryZDT(zid)))
    )

  given Arbitrary[ZonedDateTime] =
    Arbitrary(genZonedDateTime)

  given Arbitrary[Instant] =
    Arbitrary(arbitrary[ZonedDateTime].map(_.toInstant))

  given Arbitrary[Duration] =
    Arbitrary(Gen.posNum[Long].map(Duration.ofMillis))

  given Arbitrary[SDuration] = Arbitrary {
    for {
      d <- arbitrary[Duration]
    } yield SDuration.fromNanos(d.getNano.toDouble)
  }

  given Cogen[SDuration] =
    Cogen[Long].contramap(_.toNanos)

  given Cogen[Instant] =
    Cogen[(Long, Int)].contramap(t => (t.getEpochSecond, t.getNano))

  given Cogen[LocalDate] =
    Cogen[(Int, Int)].contramap(d => (d.getYear, d.getDayOfYear))

  given Cogen[Duration] =
    Cogen[(Long, Int)].contramap(d => (d.getSeconds, d.getNano))

  given Cogen[Year] =
    Cogen[Int].contramap(_.getValue)

  given Cogen[ZoneId] =
    Cogen[String].contramap(_.getId)
}

object ArbTime extends ArbTime
