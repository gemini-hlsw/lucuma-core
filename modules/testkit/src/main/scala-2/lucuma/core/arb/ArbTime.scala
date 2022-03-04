// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.arb

import cats.syntax.all._
import lucuma.core.syntax.time._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck._
import org.typelevel.cats.time._

import java.time._
import scala.concurrent.duration.{ Duration => SDuration }
import scala.jdk.CollectionConverters._

// Arbitrary but reasonable dates and times.
trait ArbTime {

  implicit val arbZoneId: Arbitrary[ZoneId] =
    Arbitrary {
      oneOf(ZoneId.getAvailableZoneIds.asScala.toSeq).map(ZoneId.of)
    }

  implicit val arbYear: Arbitrary[Year] =
    Arbitrary {
      choose(2000, 2020).map(Year.of)
    }

  implicit val arbLocalDate: Arbitrary[LocalDate] =
    Arbitrary {
      for {
        y <- arbitrary[Year]
        d <- choose(1, y.length)
      } yield LocalDate.ofYearDay(y.getValue, d)
    }

  implicit val arbLocalTime: Arbitrary[LocalTime] =
    Arbitrary {
      for {
        h <- choose(0, 23)
        m <- choose(0, 59)
        s <- choose(0, 59)
        n <- choose(0, 999999999)
      } yield LocalTime.of(h, m, s, n)
    }

  implicit val arbLocalDateTime: Arbitrary[LocalDateTime] =
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

  implicit val arbZonedDateTime: Arbitrary[ZonedDateTime] =
    Arbitrary(genZonedDateTime)

  implicit val arbInstant: Arbitrary[Instant] =
    Arbitrary(arbitrary[ZonedDateTime].map(_.toInstant))

  implicit val arbDuration: Arbitrary[Duration] =
    Arbitrary(Gen.posNum[Long].map(Duration.ofMillis))

  implicit val arbSDuration: Arbitrary[SDuration] = Arbitrary {
    for {
      d <- arbitrary[Duration]
    } yield SDuration.fromNanos(d.getNano.toDouble)
  }

  implicit val cogSDuration: Cogen[SDuration] =
    Cogen[Long].contramap(_.toNanos)

  implicit val cogInstant: Cogen[Instant] =
    Cogen[(Long, Int)].contramap(t => (t.getEpochSecond, t.getNano))

  implicit val cogLocalDate: Cogen[LocalDate] =
    Cogen[(Int, Int)].contramap(d => (d.getYear, d.getDayOfYear))

  implicit val cogDuration: Cogen[Duration] =
    Cogen[(Long, Int)].contramap(d => (d.getSeconds, d.getNano))

  implicit val cogYear: Cogen[Year] =
    Cogen[Int].contramap(_.getValue)

  implicit val cogZoneId: Cogen[ZoneId] =
    Cogen[String].contramap(_.getId)
}

object ArbTime extends ArbTime
