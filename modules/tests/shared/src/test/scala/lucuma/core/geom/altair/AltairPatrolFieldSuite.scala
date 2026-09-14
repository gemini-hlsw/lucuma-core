// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.altair

import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ScienceAreaGeometrySuite
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

class AltairPatrolFieldSuite extends ScienceAreaGeometrySuite:

  private def at(p: Double, q: Double): Offset =
    Offset.signedDecimalArcseconds.reverseGet((p, q))

  test("patrol field spans 50\" in p and 50\" in q (27\" up, 23\" down)"):
    val (p, q) = sides(patrolField.patrolField)
    assertCloseArcsec(p, 50.0)
    assertCloseArcsec(q, 50.0)
    val b      = patrolField.patrolField.eval.boundingOffsets
    assertCloseArcsec(b.topLeft.q.toAngle, 27.0)
    assertCloseArcsec(b.bottomRight.q.toAngle, -23.0)

  test("patrol field is taller towards positive q"):
    assert(patrolField.patrolField.contains(at(0.0, 26.0)))
    assert(!patrolField.patrolField.contains(at(0.0, -26.0)))
    assert(patrolField.patrolField.contains(at(0.0, -22.0)))

  test("patrol field reaches 25\" along p on both sides"):
    assert(patrolField.patrolField.contains(at(24.0, 0.0)))
    assert(patrolField.patrolField.contains(at(-24.0, 0.0)))
    assert(!patrolField.patrolField.contains(at(26.0, 0.0)))
    assert(!patrolField.patrolField.contains(at(-26.0, 0.0)))

  test("patrol field rotates with the position angle"):
    val rotated = patrolField.patrolFieldAt(Angle.Angle180, Offset.Zero)
    assert(rotated.contains(at(0.0, -26.0)))
    assert(!rotated.contains(at(0.0, 26.0)))

  test("candidates area is the 54\" circle covering the oval at any position angle"):
    val (p, q) = sides(GuideProbe.AltairAOWFS.candidatesArea)
    assertCloseArcsec(p, 54.0)
    assertCloseArcsec(q, 54.0)
