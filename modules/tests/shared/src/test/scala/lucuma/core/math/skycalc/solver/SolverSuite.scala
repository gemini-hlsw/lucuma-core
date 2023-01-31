// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc.solver

import cats.syntax.all.*
import lucuma.core.math.IntervalGens
import lucuma.core.math.skycalc.solver.RoundStrategy.*
import lucuma.core.math.skycalc.solver.SolverStrategy.*
import org.typelevel.cats.time.*
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant

final class SolverSuite extends munit.DisciplineSuite with IntervalGens {

  val TestCalculator = Samples.single(Instant.MIN, ())

  case class TestConstraint(f: Instant => Boolean) extends Constraint[Unit, Unit] {
    override def metAt[R](calc: Samples[Unit])(i: Instant)(implicit
      rounder:                  SampleRounder[R, Unit]
    ): Boolean = f(i)
  }

  implicit val testValueRounder: SampleRounder[Closest, Unit] = new SampleRounder[Closest, Unit] {
    def round(
      leftI:  Instant,
      leftV:  Unit,
      rightI: Instant,
      rightV: Unit,
      i:      Instant
    ): Option[Unit] =
      Some(())
  }

  def constraintSolver[S](f: Instant => Boolean)(implicit solver: Solver[S]) =
    ConstraintSolver[S, Closest](TestConstraint(f), Duration.ofMillis(1))

  def f1(i: Instant) =
    if (i.toEpochMilli < 150) true
    else if (i.toEpochMilli >= 250 && i.toEpochMilli < 450) true
    else false

  test("Check Default Solver") {
    val solver = constraintSolver[Default](f1)
    val solve  = solver.solve(TestCalculator) _

    assertEquals(IntervalSeq(buildInterval(0, 150)), solve(buildInterval(0, 200)))
    assertEquals(IntervalSeq(buildInterval(250, 400)), solve(buildInterval(200, 400)))
    assertEquals(IntervalSeq(buildInterval(250, 450)), solve(buildInterval(200, 500)))
    assertEquals(
      IntervalSeq(buildInterval(0, 150)) | buildInterval(250, 400),
      solve(buildInterval(0, 400))
    )
  }

  test("Check Parabola Solver") {
    val solver = constraintSolver[Parabola](f1)
    val solve  = solver.solve(TestCalculator) _

    assertEquals(IntervalSeq(buildInterval(0, 150)), solve(buildInterval(0, 200)))
    assertEquals(IntervalSeq(buildInterval(250, 400)), solve(buildInterval(200, 400)))
    assertEquals(IntervalSeq(buildInterval(250, 450)), solve(buildInterval(200, 500)))
    assertEquals(
      IntervalSeq(buildInterval(0, 150)) | buildInterval(250, 400),
      solve(buildInterval(0, 400))
    )
  }

  def f2(i: Instant) =
    if (i.toEpochMilli >= 5000 && i.toEpochMilli < 6000) true
    else false

  test("Check Parabola Solver 2") {
    val solver = constraintSolver[Parabola](f2)
    val solve  = solver.solve(TestCalculator) _

    assert(
      (IntervalSeq(buildInterval(5000, 6000)) === solve(buildInterval(0, 10000)))
    )
  }
}
