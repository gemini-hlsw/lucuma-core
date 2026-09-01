// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.all.*
import lucuma.core.util.Enumerated
import munit.FunSuite

/**
 * The scheduling mode and the Target of Opportunity activation are two
 * independent axes, each carrying its meaning in its ordering, related by a
 * single compatibility rule.  All three are easy to break silently by reordering
 * a case or adding one, so they are pinned here.
 */
final class SchedulingModeSuite extends FunSuite:

  import SchedulingMode.*

  private val ladder = List(Unconstrained, NoSplitting, Uninterruptible)

  private val activations =
    List(TooActivation.None, TooActivation.Standard, TooActivation.Rapid, TooActivation.Interrupting)

  test("the ladder is the declaration order"):
    assertEquals(Enumerated[SchedulingMode].all, ladder)

  test("the activations are the declaration order"):
    assertEquals(Enumerated[TooActivation].all, activations)

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

  test("a Target of Opportunity is exactly an activation above None"):
    assertEquals(activations.filter(_.isToo), activations.filter(_ =!= TooActivation.None))

  test("only Rapid and Interrupting oblige the observation to be Uninterruptible"):
    assertEquals(
      activations.filter(_.requiresUninterruptible),
      List(TooActivation.Rapid, TooActivation.Interrupting)
    )

  // Eight of the twelve pairings are legal.  The four that are not are exactly
  // the two most disruptive activations over the two modes that permit
  // interruption.
  test("compatibility is the whole of the relationship between the axes"):
    val legal = for a <- activations; m <- ladder if a.isCompatibleWith(m) yield (a, m)
    assertEquals(
      legal,
      List(
        (TooActivation.None,         Unconstrained),
        (TooActivation.None,         NoSplitting),
        (TooActivation.None,         Uninterruptible),
        (TooActivation.Standard,     Unconstrained),
        (TooActivation.Standard,     NoSplitting),
        (TooActivation.Standard,     Uninterruptible),
        (TooActivation.Rapid,        Uninterruptible),
        (TooActivation.Interrupting, Uninterruptible)
      )
    )

  test("preemption is exactly Interrupting over the interruptible modes"):
    val pairs = for a <- activations; m <- ladder if a.canPreempt(m) yield (a, m)
    assertEquals(pairs, List((TooActivation.Interrupting, Unconstrained), (TooActivation.Interrupting, NoSplitting)))

  test("no Target of Opportunity can preempt another"):
    // Every legally configured Rapid or Interrupting ToO is Uninterruptible, so
    // nothing can displace it -- the Scheduler never chooses between two of them
    // mid-execution.  A Standard ToO is displaceable, which is correct: it asked
    // to be observed whenever convenient.
    val legalDisruptive =
      for a <- activations; m <- ladder if a.isCompatibleWith(m) && a.requiresUninterruptible yield (a, m)
    legalDisruptive.foreach: (a, m) =>
      assert(!TooActivation.Interrupting.canPreempt(m), s"an interrupting ToO preempted $a in $m")
