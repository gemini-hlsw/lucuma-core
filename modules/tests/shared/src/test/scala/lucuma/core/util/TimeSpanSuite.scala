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
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimeSpan.genDuration
import lucuma.core.util.arb.ArbTimeSpan.genTimeSpanString
import monocle.law.discipline.IsoTests
import monocle.law.discipline.PrismTests
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Duration


class TimeSpanSuite extends DisciplineSuite {

  import ArbTimeSpan.given

  checkAll("TimeSpan.NonNegMicroseconds", IsoTests(TimeSpan.NonNegMicroseconds))
  checkAll("TimeSpan.FromMicroseconds",   PrismTests(TimeSpan.FromMicroseconds))
  checkAll("TimeSpan.FromMilliseconds",   FormatTests(TimeSpan.FromMilliseconds).format)
  checkAll("TimeSpan.FromSeconds",        FormatTests(TimeSpan.FromSeconds).format)
  checkAll("TimeSpan.FromMinutes",        FormatTests(TimeSpan.FromMinutes).format)
  checkAll("TimeSpan.FromHours",          FormatTests(TimeSpan.FromHours).format)
  checkAll("TimeSpan.FromDuration",       FormatTests(TimeSpan.FromDuration).formatWith(genDuration))
  checkAll("TimeSpan.FromString",         FormatTests(TimeSpan.FromString).formatWith(genTimeSpanString))
  checkAll("TimeSpan.Order",              OrderTests[TimeSpan].order)

}
