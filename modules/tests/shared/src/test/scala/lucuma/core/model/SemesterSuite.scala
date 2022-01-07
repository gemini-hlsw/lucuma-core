// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline._
import lucuma.core.arb.ArbTime._
import lucuma.core.enum.Half
import lucuma.core.enum.Site
import lucuma.core.model.arb.ArbSemester._
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Prop._
import org.typelevel.cats.time._

import java.time.Year
import java.time.ZoneId
import java.time.format.DateTimeFormatter

final class SemesterSuite extends munit.DisciplineSuite {
  // Laws
  checkAll("Semester", OrderTests[Semester].order)

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
      assertEquals(Semester.fromString(sʹ).map(_.format),  Some(sʹ))
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
      assertEquals(Semester.fromString(s.format),  Some(s))
    }
  }

  test(".format should also be invertible via .unsafeFromString") {
    forAll { (s: Semester) =>
      assertEquals(Semester.unsafeFromString(s.format),  s)
    }
  }

  test(".plusYears must result in a properly updated year") {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      assertEquals(sʹ.year.getValue,  s.year.getValue + nʹ)
    }
  }

  test(".plusYears should never affect the half") {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      val sʹ = s.plusYears(nʹ)
      assertEquals(sʹ.half,  s.half)
    }
  }

  test(".plusYears should have an identity") {
    forAll { (s: Semester) =>
      assertEquals(s.plusYears(0),  s)
    }
  }

  test(".plusYears should be invertible by negation, for reasonable values") {
    forAll { (s: Semester, n: Short) =>
      val nʹ = n.toInt
      assertEquals(s.plusYears(nʹ).plusYears(-nʹ),  s)
    }
  }

  test(".plusYears should be a homomorphism over addition, for reasonable values") {
    forAll { (s: Semester, n1: Short, n2: Short) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      assertEquals(s.plusYears(n1ʹ).plusYears(n2ʹ),  s.plusYears(n1ʹ + n2ʹ))
    }
  }

  test(".plusSemesters must be consistent with .plusYears, for reasonable values") {
    forAll { (s: Semester, n: Byte, b: Boolean) =>
      val nʹ = n.toInt
      val bʹ = if (b) nʹ.sign else 0
      val s1 = s.plusYears(nʹ).plusSemesters(bʹ)
      val s2 = s.plusSemesters(nʹ * 2 + bʹ)
      assertEquals(s1,  s2)
    }
  }

  test(".plusSemesters should have an identity") {
    forAll { (s: Semester) =>
      assertEquals(s.plusSemesters(0),  s)
    }
  }

  test(".plusSemesters should be invertible by negation, for reasonable values") {
    forAll { (s: Semester, n: Byte) =>
      val nʹ = n.toInt
      assertEquals(s.plusSemesters(nʹ).plusSemesters(-nʹ),  s)
    }
  }

  test(".plusSemesters should be a homomorphism over addition, for reasonable values") {
    forAll { (s: Semester, n1: Byte, n2: Byte) =>
      val (n1ʹ, n2ʹ) = (n1.toInt, n2.toInt)
      assertEquals(s.plusSemesters(n1ʹ).plusSemesters(n2ʹ),  s.plusSemesters(n1ʹ + n2ʹ))
    }
  }

  test(".start round-tripping must be consistent for .yearMonth     ~ .fromYearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.start.yearMonth),  s)
    }
  }

  test(".start should be consistent for .localDate     ~ .fromLocalDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.start.localDate),  s)
    }
  }

  test(".start should be consistent for .localDateTime ~ .fromLocalDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.start.localDateTime),  s)
    }
  }

  test(".start should be consistent for .zonedDateTime ~ .fromZonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.start.zonedDateTime(z)),  s)
    }
  }

  test(".start should be consistent for .atSite        ~ .fromSiteAndInstant") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(Semester.fromSiteAndInstant(site, s.start.atSite(site).toInstant),  s)
    }
  }

  test(".start precision must be correct for .yearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.start.yearMonth.minusMonths(1)),  s.prev)
    }
  }

  test(".start should be correct for .localDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.start.localDate.minusDays(1)),  s.prev)
    }
  }

  test(".start should be correct for .localDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.start.localDateTime.minusNanos(1)),  s.prev)
    }
  }

  test(".start should be correct for .zonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.start.zonedDateTime(z).minusNanos(1)),  s.prev)
    }
  }

  test(".start should be correct for .atSite") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(Semester.fromSiteAndInstant(site,
                   s.start.atSite(site).minusNanos(1).toInstant),
      s.prev)
    }
  }

  test(".end round-tripping must be consistent for .yearMonth     ~ .fromYearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.end.yearMonth),  s)
    }
  }

  test(".end should be consistent for .localDate     ~ .fromLocalDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.end.localDate),  s)
    }
  }

  test(".end should be consistent for .localDateTime ~ .fromLocalDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.end.localDateTime),  s)
    }
  }

  test(".end should be consistent for .zonedDateTime ~ .fromZonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.end.zonedDateTime(z)),  s)
    }
  }

  test(".end should be consistent for .atSite        ~ .fromSiteAndInstant") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(Semester.fromSiteAndInstant(site, s.end.atSite(site).toInstant),  s)
    }
  }

  test(".end precisiom must be correct for .yearMonth") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromYearMonth(s.end.yearMonth.plusMonths(1)),  s.next)
    }
  }

  test(".end should be correct for .localDate") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDate(s.end.localDate.plusDays(1)),  s.next)
    }
  }

  test(".end should be correct for .localDateTime") {
    forAll { (s: Semester) =>
      assertEquals(Semester.fromLocalDateTime(s.end.localDateTime.plusNanos(1)),  s.next)
    }
  }

  test(".end should be correct for .zonedDateTime") {
    forAll { (s: Semester, z: ZoneId) =>
      assertEquals(Semester.fromZonedDateTime(s.end.zonedDateTime(z).plusNanos(1)),  s.next)
    }
  }

  test(".end should be correct for .atSite") {
    forAll { (s: Semester, site: Site) =>
      assertEquals(Semester.fromSiteAndInstant(site,
                   s.end.atSite(site).plusNanos(1).toInstant),
      s.next)
    }
  }

}
