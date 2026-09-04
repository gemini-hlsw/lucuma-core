// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.all.*
import lucuma.core.util.Enumerated
import munit.FunSuite

/**
 * The scheduling mode carries its meaning in its ordering, and the Target of
 * Opportunity activation is derived from it. Both are easy to break silently by
 * reordering a case or adding one, so they are pinned here.
 */
final class SchedulingModeSuite extends FunSuite:

  import SchedulingMode.*

  private val ladder = List(Unconstrained, NoSplitting, Uninterruptible, Interrupting)

  test("the ladder is the declaration order"):
    assertEquals(Enumerated[SchedulingMode].all, ladder)

  test("each rung keeps every restriction below it"):
    // Splittable implies interruptible: an observation willing to be delivered
    // across visits is willing to be stopped between them.
    ladder.foreach: m =>
      assert(!m.isSplittable || m.isInterruptible, s"$m is splittable but not interruptible")

    // Restrictions only accumulate going up: no higher rung is looser than the
    // one below it.
    ladder.zip(ladder.tail).foreach: (lower, higher) =>
      assert(!higher.isSplittable || lower.isSplittable, s"$higher splittable but $lower is not")
      assert(!higher.isInterruptible || lower.isInterruptible, s"$higher interruptible but $lower is not")

  test("only Interrupting may interrupt, and it is itself uninterruptible"):
    assertEquals(ladder.filter(_.mayInterrupt), List(Interrupting))
    assert(!Interrupting.isInterruptible)

  test("an interrupting observation can never preempt another one"):
    val aggressors = ladder.filter(_.mayInterrupt)
    aggressors.foreach: a =>
      aggressors.foreach: v =>
        assert(!a.canPreempt(v), s"$a preempted $v")

  test("preemption is exactly Interrupting over the interruptible modes"):
    val pairs = for a <- ladder; v <- ladder if a.canPreempt(v) yield (a, v)
    assertEquals(pairs, List((Interrupting, Unconstrained), (Interrupting, NoSplitting)))

  // Nothing here covers the observation that has no opportunity target: it has no activation to
  // derive, so the question never reaches TooActivation and the answer is the caller's to give.
  test("activation follows the mode"):
    assertEquals(
      ladder.map(TooActivation.fromSchedulingMode),
      List(TooActivation.Standard, TooActivation.Standard, TooActivation.Rapid, TooActivation.Interrupting)
    )

  test("activation is monotone in the mode"):
    val activations = ladder.map(TooActivation.fromSchedulingMode)
    activations.zip(activations.tail).foreach: (lower, higher) =>
      assert(lower <= higher, s"$higher is less disruptive than $lower")
