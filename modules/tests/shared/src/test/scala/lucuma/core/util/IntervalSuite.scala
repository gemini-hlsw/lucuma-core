// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.kernel.laws.discipline.*
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.arb.ArbTime.arbDuration
import lucuma.core.optics.laws.discipline.*
import lucuma.core.util.arb.ArbInterval
import lucuma.core.util.arb.ArbInterval.genDuration
import lucuma.core.util.arb.ArbInterval.genIntervalString
import monocle.law.discipline.IsoTests
import monocle.law.discipline.PrismTests
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Duration


class IntervalSuite extends DisciplineSuite {

  import ArbInterval.given

  checkAll("Interval.NonNegMicroseconds", IsoTests(Interval.NonNegMicroseconds))
  checkAll("Interval.FromMicroseconds",   PrismTests(Interval.FromMicroseconds))
  checkAll("Interval.FromMilliseconds",   FormatTests(Interval.FromMilliseconds).format)
  checkAll("Interval.FromSeconds",        FormatTests(Interval.FromSeconds).format)
  checkAll("Interval.FromMinutes",        FormatTests(Interval.FromMinutes).format)
  checkAll("Interval.FromHours",          FormatTests(Interval.FromHours).format)
  checkAll("Interval.FromDuration",       FormatTests(Interval.FromDuration).formatWith(genDuration))
  checkAll("Interval.FromString",         FormatTests(Interval.FromString).formatWith(genIntervalString))
  checkAll("Interval.Order",              OrderTests[Interval].order)

}
