// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import cats.implicits._
import gsp.math.skycalc.solver.Interval
import org.scalacheck._
import org.scalacheck.Arbitrary._
import java.time.Instant
import io.chrisdavenport.cats.time._

trait ArbInterval {
  import ArbTime._

  implicit val arbInterval: Arbitrary[Interval] =
    Arbitrary(
      for {
        a <- arbitrary[Instant]
        b <- arbitrary[Instant].suchThat(_ =!= a)
      } yield {
        val ab = List(a, b).sorted
        Interval.unsafe(ab(0), ab(1))
      }
    )

  implicit val cogenInterval: Cogen[Interval] =
    Cogen[(Instant, Instant)].contramap(i => (i.start, i.end))
}

object ArbInterval extends ArbInterval
