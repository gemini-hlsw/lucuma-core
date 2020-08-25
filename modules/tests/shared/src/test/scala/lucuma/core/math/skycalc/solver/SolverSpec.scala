// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.Instant
import java.time.Duration
import gsp.math.skycalc.solver.SolverStrategy._
import gsp.math.skycalc.solver.GetterStrategy._

final class SolverSpec extends CatsSuite {

  val TestCalculator = Samples.single(Instant.MIN, ())

  case class TestConstraint(f: Instant => Boolean) extends Constraint[Unit, Unit] {
    override def metAt[G](calc: Samples[Unit])(i: Instant)(implicit
      getter:                   CalcGetter[G, Unit]
    ): Boolean = f(i)
  }

  implicit val testValueGetter = new CalcGetter[Closest, Unit] {
    def get(calc: Samples[Unit])(
      instant:       Instant
    ): Option[Unit] = Some(())
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

    assert(Schedule(List(buildInterval(0, 150))) === solve(buildInterval(0, 200)).some)
    assert(Schedule(List(buildInterval(250, 400))) === solve(buildInterval(200, 400)).some)
    assert(Schedule(List(buildInterval(250, 450))) === solve(buildInterval(200, 500)).some)
    assert(
      Schedule(List(buildInterval(0, 150), buildInterval(250, 400))) === solve(
        buildInterval(0, 400)
      ).some
    )
  }

  test("Check Parabola Solver") {
    val solver = constraintSolver[Parabola](f1)
    val solve  = solver.solve(TestCalculator) _

    assert(Schedule(List(buildInterval(0, 150))) === solve(buildInterval(0, 200)).some)
    assert(Schedule(List(buildInterval(250, 400))) === solve(buildInterval(200, 400)).some)
    assert(Schedule(List(buildInterval(250, 450))) === solve(buildInterval(200, 500)).some)
    assert(
      Schedule(List(buildInterval(0, 150), buildInterval(250, 400))) === solve(
        buildInterval(0, 400)
      ).some
    )
  }

  def f2(i: Instant) =
    if (i.toEpochMilli >= 5000 && i.toEpochMilli < 6000) true
    else false

  test("Check Parabola Solver 2") {
    val solver = constraintSolver[Parabola](f2)
    val solve  = solver.solve(TestCalculator) _

    assert(
      (Schedule(List(buildInterval(5000, 6000))) === solve(buildInterval(0, 10000)).some)
    )
  }
}
