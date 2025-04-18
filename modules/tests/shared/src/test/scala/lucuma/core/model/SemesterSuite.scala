// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.arb.ArbTime.given
import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.model.arb.ArbSemester.given
import lucuma.core.util.arb.ArbEnumerated.given
import monocle.law.discipline.*
import org.scalacheck.Prop.*
import org.typelevel.cats.time.*

import java.time.Year
import java.time.ZoneId
import java.time.format.DateTimeFormatter

final class SemesterSuite extends munit.DisciplineSuite {
  // Laws
  checkAll("Semester", OrderTests[Semester].order)
  checkAll("Semester.fromString", PrismTests(Semester.fromString))

  test("Equality must be natural") {
    forAll { (a: Semester, b: Semester) =>
      assertEquals(a.equals(b),  Eq[Semester].eqv(a, b))
    }
  }

  test("Equality must operate pairwise") {
    forAll { (a: Semester, b: Semester) =>
      assertEquals(Eq[Year].eqv(a.year, b.year) &&
        Eq[Half].eqv(a.half, b.half),  Eq[Semester].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Semester) =>
      assertEquals(a.toString,  Show[Semester].show(a))
    }
  }

  test(".fromString must be invertible via .format") {
    forAll { (y: Year, h: Half) =>
      val yʹ = DateTimeFormatter.ofPattern("yyyy").format(y)
      val hʹ = h.tag
      val sʹ = s"$yʹ$hʹ"
      assertEquals(Semester.fromString.getOption(sʹ).map(_.format),  sʹ.some)
    }
  }

  test(".unsafeFromString must be invertible via .format") {
    forAll { (y: Year, h: Half) =>
      val yʹ = DateTimeFormatter.ofPattern("yyyy").format(y)
      val hʹ = h.tag
      val sʹ = s"$yʹ$hʹ"
      assertEquals(Semester.unsafeFromString(sʹ).format,  sʹ)
    }
  }

  test(".format must be invertible via .fromString") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromString.getOption(s.format),  s.some)
    }
  }

  test(".format should also be invertible via .unsafeFromString") {
    forAll { (s: Semester) =>
      assertEquals(Semester.unsafeFromString(s.format),  s)
    }
  }

  def inRange(y: Int): Boolean =
    Semester.YearInt.MinValue.value <= y && y <= Semester.YearInt.MaxValue.value

  test(".plusYears must result in a properly updated year") {
    forAll { (s: Semester, n: Short) =>
      val y = s.yearInt.value + n
      s.plusYears(n) match {
        case Some(sʹ) => assertEquals(sʹ.year.getValue,  y)
        case None     => assert(!inRange(y))
      }
    }
  }

  test(".plusYears should never affect the half") {
    forAll { (s: Semester, n: Short) =>
      assert(s.plusYears(n).map(_.half).forall(_ === s.half))
    }
  }

  test(".plusYears should have an identity") {
    forAll { (s: Semester) =>
      assert(s.plusYears(0).exists(_ === s))
    }
  }

  test(".plusYears should be invertible by negation, for reasonable values") {
    forAll { (s: Semester, n: Short) =>
      assert(s.plusYears(n).flatMap(_.plusYears(-n)).forall(_ === s))
    }
  }

  test(".plusSemesters must be consistent with .plusYears, for reasonable values") {
    forAll { (s: Semester, n: Byte, b: Boolean) =>
      val bʹ = if (b) n.sign else 0
      val s1 = s.plusYears(n).flatMap(_.plusSemesters(bʹ))
      val s2 = s.plusSemesters(n * 2 + bʹ)
      assertEquals(s1,  s2)
    }
  }

  test(".plusSemesters should have an identity") {
    forAll { (s: Semester) =>
      assert(s.plusSemesters(0).exists(_ === s))
    }
  }

  test(".plusSemesters should be invertible by negation, for reasonable values") {
    forAll { (s: Semester, n: Byte) =>
      assert(s.plusSemesters(n).flatMap(_.plusSemesters(-n)).forall(_ === s))
    }
  }

  test(".start round-tripping must be consistent for .yearMonth     ~ .fromYearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.start.yearMonth), s.some)
    }
  }

  test(".start should be consistent for .localDate     ~ .fromLocalDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.start.localDate), s.some)
    }
  }

  test(".start should be consistent for .localDateTime ~ .fromLocalDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.start.localDateTime), s.some)
    }
  }

  test(".start should be consistent for .zonedDateTime ~ .fromZonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.start.zonedDateTime(z)), s.some)
    }
  }

  test(".start should be consistent for .atSite        ~ .fromSiteAndInstant") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(Semester.fromSiteAndInstant(site, s.start.atSite(site).toInstant), s.some)
    }
  }

  test(".start precision must be correct for .yearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.start.yearMonth.minusMonths(1)), s.prev)
    }
  }

  test(".start should be correct for .localDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.start.localDate.minusDays(1)), s.prev)
    }
  }

  test(".start should be correct for .localDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.start.localDateTime.minusNanos(1)), s.prev)
    }
  }

  test(".start should be correct for .zonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.start.zonedDateTime(z).minusNanos(1)), s.prev)
    }
  }

  test(".start should be correct for .atSite") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(
        Semester.fromSiteAndInstant(site, s.start.atSite(site).minusNanos(1).toInstant),
        s.prev
      )
    }
  }

  test(".end round-tripping must be consistent for .yearMonth     ~ .fromYearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.end.yearMonth), s.some)
    }
  }

  test(".end should be consistent for .localDate     ~ .fromLocalDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.end.localDate), s.some)
    }
  }

  test(".end should be consistent for .localDateTime ~ .fromLocalDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.end.localDateTime), s.some)
    }
  }

  test(".end should be consistent for .zonedDateTime ~ .fromZonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.end.zonedDateTime(z)), s.some)
    }
  }

  test(".end should be consistent for .atSite        ~ .fromSiteAndInstant") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(Semester.fromSiteAndInstant(site, s.end.atSite(site).toInstant), s.some)
    }
  }

  test(".end precisiom must be correct for .yearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.end.yearMonth.plusMonths(1)), s.next)
    }
  }

  test(".end should be correct for .localDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.end.localDate.plusDays(1)), s.next)
    }
  }

  test(".end should be correct for .localDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.end.localDateTime.plusNanos(1)), s.next)
    }
  }

  test(".end should be correct for .zonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.end.zonedDateTime(z).plusNanos(1)),  s.next)
    }
  }

  test(".end should be correct for .atSite") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(
        Semester.fromSiteAndInstant(site, s.end.atSite(site).plusNanos(1).toInstant),
        s.next
      )
    }
  }

}
