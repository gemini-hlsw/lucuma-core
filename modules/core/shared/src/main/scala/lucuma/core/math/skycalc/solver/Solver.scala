// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.implicits._
import java.time.Duration
import java.time.Instant
import io.chrisdavenport.cats.time._
import scala.annotation.tailrec

object SolverStrategy {
  sealed trait Default
  sealed trait Parabola
}

/**
  * Typeclass defininig an algorithm that finds a [[Schedule]] within a given [[Interval]] for which a given
  * function <code>Instant => Boolean</code> is true.
  *
  * @tparam S [[SolverStrategy]] to use
  */
trait Solver[S] {
  def solve(
    metAt:    Instant => Boolean
  )(interval: Interval, step: Duration = Duration.ofSeconds(30)): Schedule
}

trait SolverInstances {

  /**
    * Solver that finds all intervals for which the underlying constraint is true by sampling the constraint function
    * at a given rate. Intervals shorter than the sampling rate may be missed by this solver. Use this solver
    * for complex functions which can have an arbitrary amount of separate intervals for which the constraint
    * meats its criteria (e.g. Sky Background).
    */
  implicit object DefaultSolver extends Solver[SolverStrategy.Default] {
    def solve(metAt: Instant => Boolean)(interval: Interval, step: Duration): Schedule = {
      require(step > Duration.ZERO)

      @tailrec
      def solve(curStart: Instant, curState: Boolean, i: Instant, accum: Schedule): Schedule =
        if (i >= interval.end)
          if (curState)
            accum.union(Interval.unsafe(curStart, interval.end))
          else
            accum
        else if (metAt(i) == curState)
          solve(curStart, curState, i.plus(step), accum)
        else if (curState)
          solve(i, curState = false, i.plus(step), accum.union(Interval.unsafe(curStart, i)))
        else
          solve(i, curState = true, i.plus(step), accum)

      solve(interval.start, metAt(interval.start), interval.start, Schedule.Never)
    }
  }

  /**
    * Finds a solution for a constraint on a parabolic curve that crosses that constraint
    * at most twice during the given interval. This is true for all basic elevation constraints for a single night.
    */
  implicit object ParabolaSolver extends Solver[SolverStrategy.Parabola] {
    def solve(metAt: Instant => Boolean)(interval: Interval, step: Duration): Schedule = {
      require(step > Duration.ZERO)

      def solve(s: Instant, fs: Boolean, e: Instant, fe: Boolean): Schedule = {
        val m  = Instant.ofEpochMilli((s.toEpochMilli + e.toEpochMilli) / 2)
        val fm = metAt(m)
        if (Interval.unsafe(s, e).duration > step)
          (fs, fm, fe) match {
            case (false, false, false) => solve(s, fs, m, fm).union(solve(m, fm, e, fe))
            case (false, false, true)  => solve(m, fm, e, fe)
            case (false, true, false)  => solve(s, fs, m, fm).union(solve(m, fm, e, fe))
            case (false, true, true)   => solve(s, fs, m, fm).union(Schedule.unsafe(m, e))
            case (true, false, false)  => solve(s, fs, m, fm)
            case (true, false, true)   => solve(s, fs, m, fm).union(solve(m, fm, e, fe))
            case (true, true, false)   => Schedule.unsafe(s, m).union(solve(m, fm, e, fe))
            case (true, true, true)    => solve(s, fs, m, fm).union(solve(m, fm, e, fe))
          }
        else if (fm)
          Schedule.single(Interval.unsafe(s, e))
        else
          Schedule.Never
      }

      val fs = metAt(interval.start)
      val fe = metAt(interval.end)
      solve(interval.start, fs, interval.end, fe)
    }
  }
}

object Solver extends SolverInstances
