// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import java.time.Instant
import java.time.Duration

final class SolverSpec extends CatsSuite {
  class TestCalculator extends Calculator[GetterStrategy.Closest, Unit] {
    override val instants: List[Instant] = List.empty

    override def toIndex(i: Instant): Int = 0

    override val result: Instant => Unit = _ => ()
  }

  case class TestConstraint(solver: Solver[Unit], f: Instant => Boolean)
      extends Constraint[Unit, Unit] {

    override def metAt[G](calc: Calculator[G, Unit])(i: Instant)(implicit
      getter:                   CalcGetter[G, Unit]
    ): Boolean = f(i)
  }

  val testCalculator = new TestCalculator

  implicit val testValueGetter = new CalcGetter[GetterStrategy.Closest, Unit] {
    def get[T](calc: Calculator[GetterStrategy.Closest, T])(field: T => Unit)(
      instant:       Instant
    ): Unit = ()
  }

  def f1(i: Instant) =
    if (i.toEpochMilli < 150) true
    else if (i.toEpochMilli >= 250 && i.toEpochMilli < 450) true
    else false

  test("Check Default Solver") {
    val s     = DefaultSolver[Unit](Duration.ofMillis(1))
    val c     = TestConstraint(s, f1)
    val solve = c.solve(testCalculator) _

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
    val c     = TestConstraint(ParabolaSolver[Unit](Duration.ofMillis(1)), f1)
    val solve = c.solve(testCalculator) _

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
    val c     = TestConstraint(ParabolaSolver[Unit](Duration.ofMillis(1)), f2)
    val solve = c.solve(testCalculator) _

    assert(
      (Schedule(List(buildInterval(5000, 6000))) === solve(buildInterval(0, 10000)).some)
    )
  }
}
