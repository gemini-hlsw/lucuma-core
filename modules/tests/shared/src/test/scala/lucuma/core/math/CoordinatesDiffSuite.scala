// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._

import scala.math._

final class CoordinatesDiffSuite extends munit.DisciplineSuite {
  import ArbCoordinatesDiff._

  // Laws
  checkAll("Eq[CoordinatesDiff]", EqTests[CoordinatesDiff].eqv)

  val errorDelta = 0.01

  def assertCloseDifference(
    cd:       CoordinatesDiff,
    posAngle: Angle,
    distance: Angle,
    d:        Double
  ): Unit = {
    assertEqualsDouble(cd.distance.toDoubleDegrees, distance.toDoubleDegrees, d)
    assertEqualsDouble(cd.posAngle.toDoubleDegrees, posAngle.toDoubleDegrees, d)
  }

  def assertCloseOffset(o: Offset, p: Offset.P, q: Offset.Q, d: Double): Unit = {
    assertEqualsDouble(o.p.toAngle.toSignedDoubleDegrees, p.toAngle.toSignedDoubleDegrees, d)
    assertEqualsDouble(o.q.toAngle.toSignedDoubleDegrees, q.toAngle.toSignedDoubleDegrees, d)
  }

  def assertCloseToEquator(cd: CoordinatesDiff, c: Coordinates, a: Angle, delta: Double): Unit = {
    def normalize(a: Angle): Angle = if (a.toDoubleDegrees > 180) Angle.Angle0 - a else a
    val normalizedDec              = normalize(c.dec.toAngle)
    val normalizedRa               = normalize(c.ra.toAngle)

    // The distance is close enough to a straightforward calculation at the
    // equator and prime meridian. Calculated in arcsec
    val dist = sqrt(
      pow(normalizedRa.toDoubleDegrees / 3600, 2) + pow(normalizedDec.toDoubleDegrees / 3600, 2)
    )

    assertCloseDifference(cd, a, Angle.fromDoubleDegrees(dist), delta)
  }
  

  test("calculate on Z0") {
    val base     = Coordinates(RightAscension.Zero,
                           Declination.fromDoubleDegrees(90).getOrElse(Declination.Zero)
    )
    val distance = Angle.fromDMS(0, 0, 10, 0, 0)
    val dec      =
      Declination.fromDoubleDegrees(90 - distance.toDoubleDegrees).getOrElse(Declination.Zero)

    for {
      i  <- 0 to 4
      a   = Angle.fromDoubleDegrees(45.0 * i)
      c   = Coordinates(RightAscension.fromDoubleDegrees(a.toDoubleDegrees), dec)
      ref = Angle.fromDoubleDegrees(180) - a
    } yield assertCloseDifference(base.diff(c), ref, distance, errorDelta)
  }
  test("calculate offsets on Z0") {
    val base     = Coordinates(RightAscension.Zero,
                           Declination.fromDoubleDegrees(90).getOrElse(Declination.Zero)
    )
    val (dec, _) =
      Declination.fromAngleWithCarry(Angle.fromDoubleDegrees(90) - Angle.fromDMS(0, 0, 10, 0, 0))

    val results =
      List((0.0, -10.0), (7.071068, -7.071068), (10.0, 0.0), (7.071068, 7.071068), (0.0, 10.0))
    for {
      i     <- 0 to 4
      r      = results(i)
      a      = Angle.fromDoubleDegrees(45.0 * i)
      c      = Coordinates(RightAscension.fromDoubleDegrees(a.toDoubleDegrees), dec)
      offset = base.diff(c).offset
    } yield assertCloseOffset(offset,
                              Angle.fromDoubleArcseconds(r._1).p,
                              Angle.fromDoubleArcseconds(r._2).q,
                              errorDelta
    )
  }
  test("calculate difference close to the equator") {
    val base = Coordinates.Zero

    def coordinatesTest(ra: Angle, dec: Angle, qa0: Angle) = {
      val q0 = Coordinates(RightAscension.fromDoubleDegrees(ra.toDoubleDegrees),
                           Declination.fromAngleWithCarry(dec)._1
      )
      assertCloseToEquator(base.diff(q0), q0, qa0, errorDelta)

      val q1  = Coordinates(RightAscension.fromDoubleDegrees(ra.toDoubleDegrees),
                           Declination.fromAngleWithCarry(Angle.Angle0 - dec)._1
      )
      val qa1 = Angle.Angle180 - qa0
      assertCloseToEquator(base.diff(q1), q1, qa1, errorDelta)

      val q2  = Coordinates(RightAscension.fromDoubleDegrees((Angle.Angle0 - ra).toDoubleDegrees),
                           Declination.fromAngleWithCarry(Angle.Angle0 - dec)._1
      )
      val qa2 = Angle.Angle180 + qa0
      assertCloseToEquator(base.diff(q2), q2, qa2, errorDelta)

      val q3  = Coordinates(RightAscension.fromDoubleDegrees((Angle.Angle0 - ra).toDoubleDegrees),
                           Declination.fromAngleWithCarry(dec)._1
      )
      val qa3 = Angle.fromDoubleDegrees(360) - qa0
      assertCloseToEquator(base.diff(q3), q3, qa3, errorDelta)
    }

    coordinatesTest(Angle.fromDoubleDegrees(10.0 / 3600),
                    Angle.fromDoubleDegrees(10.0 / 3600),
                    Angle.fromDoubleDegrees(45)
    )
    coordinatesTest(Angle.fromDoubleDegrees(10 * 0.5 / 3600),
                    Angle.fromDoubleDegrees(10.0 * sqrt(3) / 2.0 / 3600),
                    Angle.fromDoubleDegrees(30)
    )

  }
  test("calculate offsets close to the equator") {
    val base = Coordinates.Zero

    def offsetsTest(ra: Angle, dec: Angle) = {
      val q0 = Coordinates(RightAscension.fromDoubleDegrees(ra.toDoubleDegrees),
                           Declination.fromAngleWithCarry(dec)._1
      )
      assertCloseOffset(base.diff(q0).offset, ra.p, dec.q, errorDelta)

      val q1 = Coordinates(RightAscension.fromDoubleDegrees(ra.toDoubleDegrees),
                           Declination.fromAngleWithCarry(Angle.Angle0 - dec)._1
      )
      assertCloseOffset(base.diff(q1).offset, ra.p, dec.mirrorBy(Angle.Angle180).q, errorDelta)

      val q2 = Coordinates(RightAscension.fromDoubleDegrees((Angle.Angle0 - ra).toDoubleDegrees),
                           Declination.fromAngleWithCarry(Angle.Angle0 - dec)._1
      )
      assertCloseOffset(base.diff(q2).offset,
                        ra.mirrorBy(Angle.Angle180).p,
                        dec.mirrorBy(Angle.Angle180).q,
                        errorDelta
      )

      val q3 = Coordinates(RightAscension.fromDoubleDegrees((Angle.Angle0 - ra).toDoubleDegrees),
                           Declination.fromAngleWithCarry(dec)._1
      )
      assertCloseOffset(base.diff(q3).offset, ra.mirrorBy(Angle.Angle180).p, dec.q, errorDelta)
    }

    offsetsTest(Angle.fromDoubleDegrees(10.0 / 3600), Angle.fromDoubleDegrees(10.0 / 3600))
    offsetsTest(Angle.fromDoubleDegrees(10 * 0.5 / 3600),
                Angle.fromDoubleDegrees(10.0 * sqrt(3) / 2.0 / 3600)
    )
  }

}
