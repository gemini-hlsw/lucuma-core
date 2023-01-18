// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.NonNegLong
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import java.time.Duration
import scala.util.Try

trait ArbInterval {

  given Arbitrary[Interval] =
    Arbitrary {
      arbitrary[NonNegLong].map(Interval.fromNonNegMicroseconds)
    }

  given Cogen[Interval] =
    Cogen[Long].contramap(_.toMicroseconds)

  val genDuration: Gen[Duration] =
    for {
      s  <- arbitrary[Long]
      ns <- arbitrary[Int]
    } yield Try(Duration.ofSeconds(s, ns)).getOrElse(Duration.ofSeconds(s))


  val genIntervalString: Gen[String] =
    Gen.oneOf(
      // Normal ISO 8601 duration string
      arbitrary[Interval].map(_.format),
      // For the coverage test, prepend a 0 to a time value every now and then
      arbitrary[Interval].map(_.format).map { s =>
        s.replaceFirst("([0-9]+)([DHMS])", "0$1$2")
      },
      // Make some invalid strings sometimes
      arbitrary[(Interval, Int, Char)].map { case (n, i, c) =>
        val cs = n.format.toCharArray
        val in = (i % cs.size).abs
        cs(in) = c
        String.valueOf(cs)
      }
    )
}

object ArbInterval extends ArbInterval