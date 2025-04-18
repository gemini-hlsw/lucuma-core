// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc.solver

import cats.syntax.all.*
import lucuma.core.math.BoundedInterval
import lucuma.core.syntax.time.*
import org.typelevel.cats.time.*
import spire.math.Interval
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec

object SolverStrategy {
  type Default
  type Parabola
}

/**
 * Typeclass defininig an algorithm that finds an `IntervalSeq[Instant]` within a
 * given `Bounded[Instant]` for which a given function <code>Instant => Boolean</code> is true.
 *
 * @tparam S [[SolverStrategy]] to use
 */
trait Solver[S] {
  def solve(
    metAt:    Instant => Boolean
  )(interval: BoundedInterval[Instant], step: Duration = Duration.ofSeconds(30)): IntervalSeq[Instant]
}

trait SolverInstances {

  /**
   * Solver that finds all intervals for which the underlying constraint is true by sampling the constraint function
   * at a given rate. Intervals shorter than the sampling rate may be missed by this solver. Use this solver
   * for complex functions which can have an arbitrary amount of separate intervals for which the constraint
   * meats its criteria (e.g. Sky Background).
   */
  implicit object DefaultSolver extends Solver[SolverStrategy.Default] {
    def solve(
      metAt:    Instant => Boolean
    )(interval: BoundedInterval[Instant], step: Duration): IntervalSeq[Instant] = {
      require(step > Duration.ZERO)

      @tailrec
      def solve(
        curStart: Instant,
        curState: Boolean,
        i:        Instant,
        accum:    IntervalSeq[Instant]
      ): IntervalSeq[Instant] =
        if (i >= interval.upper)
          if (curState)
            accum | Interval.openUpper(curStart, interval.upper)
          else
            accum
        else if (metAt(i) == curState)
          solve(curStart, curState, i + step, accum)
        else if (curState)
          solve(
            i,
            curState = false,
            i + step,
            accum | Interval.openUpper(curStart, i)
          )
        else
          solve(i, curState = true, i + step, accum)

      solve(interval.lower, metAt(interval.lower), interval.lower, IntervalSeq.empty[Instant])
    }
  }

  /**
   * Finds a solution for a constraint on a parabolic curve that crosses that constraint
   * at most twice during the given interval. This is true for all basic elevation constraints for a single night.
   */
  implicit object ParabolaSolver extends Solver[SolverStrategy.Parabola] {
    def solve(
      metAt:    Instant => Boolean
    )(interval: BoundedInterval[Instant], step: Duration): IntervalSeq[Instant] = {
      require(step > Duration.ZERO)

      def solve(s: Instant, fs: Boolean, e: Instant, fe: Boolean): IntervalSeq[Instant] = {
        val m  = Instant.ofEpochMilli((s.toEpochMilli + e.toEpochMilli) / 2)
        val fm = metAt(m)
        if (BoundedInterval.unsafeOpenUpper(s, e).duration > step)
          (fs, fm, fe) match {
            case (false, false, false) => solve(s, fs, m, fm) | (solve(m, fm, e, fe))
            case (false, false, true)  => solve(m, fm, e, fe)
            case (false, true, false)  => solve(s, fs, m, fm) | (solve(m, fm, e, fe))
            case (false, true, true)   =>
              solve(s, fs, m, fm) | IntervalSeq(Interval.openUpper(m, e))
            case (true, false, false)  => solve(s, fs, m, fm)
            case (true, false, true)   => solve(s, fs, m, fm) | (solve(m, fm, e, fe))
            case (true, true, false)   =>
              IntervalSeq(Interval.openUpper(s, m)) | (solve(m, fm, e, fe))
            case (true, true, true)    => solve(s, fs, m, fm) | (solve(m, fm, e, fe))
          }
        else if (fm)
          IntervalSeq[Instant](Interval.openUpper(s, e))
        else
          IntervalSeq.empty[Instant]
      }

      val fs = metAt(interval.lower)
      val fe = metAt(interval.upper)
      solve(interval.lower, fs, interval.upper, fe)
    }
  }
}

object Solver extends SolverInstances
