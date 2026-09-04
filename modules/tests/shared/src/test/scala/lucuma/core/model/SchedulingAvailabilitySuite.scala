// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.model.arb.ArbSchedulingAvailability.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan.given
import munit.*
import org.scalacheck.Prop.forAll

import java.time.Duration

final class SchedulingAvailabilitySuite extends DisciplineSuite {

  private def days(n: Long): TimeSpan  = TimeSpan.unsafeFromDuration(Duration.ofDays(n))
  private def hours(n: Long): TimeSpan = TimeSpan.unsafeFromDuration(Duration.ofHours(n))

  /** Approved at the start of a 180 day semester, withholding nothing. */
  private val WholeSemester = SchedulingAvailability.unconstrained(days(180))

  test("an observation added mid-semester and open for all that remains is subsumed"):
    // It cannot offer 180 days -- there are only 121 left -- and asking it to
    // would make every late addition unapprovable.
    assert(WholeSemester.subsumes(SchedulingAvailability.unconstrained(days(121))))

  test("a timing window that opened before it was declared cannot borrow that time"):
    // Declared with 8 hours of semester left and shutting after 4 of them.  The
    // elapsed months it nominally spans are not the PI's to offer.
    assert(!WholeSemester.subsumes(SchedulingAvailability(hours(4), hours(8))))

  test("on the last night, offering everything left is subsumed"):
    assert(WholeSemester.subsumes(SchedulingAvailability(hours(8), hours(8))))

  test("genuinely low availability is a shortening, however much semester remains"):
    assert(!WholeSemester.subsumes(SchedulingAvailability(hours(6), days(152))))

  test("widening is free"):
    val approved = SchedulingAvailability(hours(6), days(152))
    assert(approved.subsumes(SchedulingAvailability(hours(12), days(140))))

  test("shortening is not"):
    val approved = SchedulingAvailability(hours(12), days(150))
    assert(!approved.subsumes(SchedulingAvailability(hours(6), days(140))))

  test("an unrecorded minimum subsumes anything"):
    forAll: (w: SchedulingAvailability) =>
      assert(SchedulingAvailability.Zero.subsumes(w))

  test("withholding nothing is always subsumed"):
    forAll: (approved: SchedulingAvailability, remaining: TimeSpan) =>
      assert(approved.subsumes(SchedulingAvailability.unconstrained(remaining)))

  // The requirement is capped by the proposal's own remaining time, and a
  // minimum of two figures is never greater than the first, so an availability
  // always clears the bar it sets itself.
  test("an availability always subsumes itself"):
    forAll: (w: SchedulingAvailability) =>
      assert(w.subsumes(w))

}
