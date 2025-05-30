// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.kernel.laws.discipline.EqTests
import cats.syntax.all.*
import lucuma.core.math.arb.*
import monocle.law.discipline.OptionalTests
import monocle.law.discipline.PrismTests
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Prop.*

final class ArcSuite extends munit.DisciplineSuite:
  import ArbAngle.given
  import ArbArc.given
  import ArbRightAscension.given
  import ArbDeclination.given

  extension (d: Double) def positiveFractionalPart: Double =
    d.abs % 1

  val Angle1   = Angle.degrees.reverseGet(1)
  val Angle5   = Angle.degrees.reverseGet(5)
  val Angle350 = Angle.degrees.reverseGet(350)

  def mkTests[A: Angular: Arbitrary: Eq: Cogen](tpe: String): Unit =

    val label = s"Arc[$tpe]"

    checkAll(label, EqTests[Arc[A]].eqv)
    checkAll(label, PrismTests(Arc.empty[A]))
    checkAll(label, PrismTests(Arc.full[A]))
    checkAll(label, PrismTests(Arc.partial[A]))
    checkAll(label, OptionalTests(Arc.start[A]))
    checkAll(label, OptionalTests(Arc.end[A]))

    test(s"$label: Equality must be natural"):
      forAll: (a: Arc[A], b: Arc[A]) =>
        assertEquals(a.equals(b), Eq[Arc[A]].eqv(a, b))

    test(s"$label: contains() must be consistent for A and Angle."):
      forAll: (a: Arc[A], b: A) =>
        assertEquals(a.contains(b), a.contains(b.toAngle))

    test(s"$label: The empty arc contains no elements."):
      forAll: (a: A) =>
        assert(!Arc.Empty().contains(a))

    test(s"$label: The full arc contains every element."):
      forAll: (a: A) =>
        assert(Arc.Full().contains(a))
    
    test(s"$label: Partial arcs contain both endpoints."):
      forAll: (a: Arc.Partial[A]) =>
        assert(a.contains(a.start))
        assert(a.contains(a.end))

    test(s"$label: Every arc containsAll of itself"):
      forAll: (a: Arc[A]) =>
        assert(a.containsAll(a))              

    test(s"$label: Every arc containsAll of the empty arc."):
      forAll: (a: Arc[A]) =>
        assert(a.containsAll(Arc.Empty()))              

    test(s"$label: The empty arc containsAll of no non-empty arc."):
      forAll: (a: Arc[A]) =>
        a.nonEmpty ==> assert(!Arc.Empty().containsAll(a))

    test(s"$label: The full arc containsAll of every arc."):
      forAll: (a: Arc[A]) =>
        assert(Arc.Full().containsAll(a))

    test(s"$label: No non-full arc containAll of the full arc."):
      forAll: (a: Arc[A]) =>
        a.nonFull ==> assert(!a.containsAll(Arc.Full()))              

    test(s"$label: existsOverlap commutes."):
      forAll: (a: Arc[A], b: Arc[A]) =>
        assertEquals(a.existsOverlap(b), b.existsOverlap(a))

    test(s"$label: There existsOverlap between every non-empty arc and itself"):
      forAll: (a: Arc[A]) =>
        a.nonEmpty ==> assert(a.containsAll(a))              
    
    test(s"$label: There existsOverlap between all non-empty arcs the full arc."):
      forAll: (a: Arc[A]) =>
        a.nonEmpty ==> assert(a.existsOverlap(Arc.Full()))

    test(s"$label: There never existsOverlap with the empty arc."):
      forAll: (a: Arc[A]) =>
        assert(!a.existsOverlap(Arc.Empty()))
    
  mkTests[RightAscension]("RightAscension")
  mkTests[Declination]("Declination")
  mkTests[Angle]("Angle")

  test("Arc[Angle]: Partial arc should contain points between start and end (clockwise)."):
    forAll: (a: Arc.Partial[Angle]) =>
      (0 to 100).map(_ * 0.01).foreach: f =>
        val p = a.start + a.size * f
        assert(a.contains(p))

  test("Arc[Angle]: Partial arc should not contains points between end and start (clockwise)."):
    forAll: (a: Arc.Partial[Angle]) =>
      val outsize = -a.size
      a.nonSingular ==> 
        (0 to 100).map(_ * 0.01).foreach: f =>
          val p = a.end + outsize * f
          if p != a.start && p != a.end then
            assert(!a.contains(p))

  test("Arc[Angle]: Partial arc should containAll of arc created with two interior points."):
    forAll: (a: Arc.Partial[Angle], d1: Double, d2: Double) =>
      given Order[Angle] = Angle.AngleOrder
      (a.size > Angle5 && a.size < Angle350) ==> {
        val s = Arc.Partial(a.start + Angle1, a.end - Angle1)
        assert(a.containsAll(s))
      }

  test("Arc[Angle]: There existsOverlap between an partial arc and any other that contains one of its interior points."):
    forAll: (a: Arc.Partial[Angle], d1: Double, e: Angle) =>
      val s = a.start + a.size * d1.positiveFractionalPart
      assert(a.existsOverlap(Arc.Partial(s, e)))
    
  test("Arc[Angle]: Partial arc should not overlap an arc created with two exterior points."):
    given Order[Angle] = Angle.AngleOrder
    forAll: (a: Arc.Partial[Angle], d1: Double, d2: Double) =>
      (a.size > Angle5 && a.size < Angle350) ==> {
        val s = Arc.Partial(a.end + Angle1, a.start - Angle1)
        assert(!a.existsOverlap(s))
      }
    