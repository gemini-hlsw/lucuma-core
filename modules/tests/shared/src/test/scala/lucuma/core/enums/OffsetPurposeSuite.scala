// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.enums.OffsetPurpose.Object
import lucuma.core.enums.OffsetPurpose.Sky
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gnirs
import lucuma.core.geom.igrins2
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.sequence.gnirs.GnirsFpu
import munit.FunSuite

class OffsetPurposeSuite extends FunSuite:

  private def purpose(area: ShapeExpression, p: BigDecimal, q: BigDecimal): OffsetPurpose =
    OffsetPurpose.fromScienceArea(area)(Offset(p.pArcsec, q.qArcsec))

  // IGRINS-2 slit: 0.3" wide (p) by 5" long (q)
  private val igrins2Slit: ShapeExpression =
    igrins2.scienceArea.scienceSlitFOV

  test("IGRINS-2: the base position is Object (e.g., an unguided sky calibration)"):
    assertEquals(purpose(igrins2Slit, 0, 0), Object)

  test("IGRINS-2: nodding along the slit stays on the object"):
    assertEquals(purpose(igrins2Slit, 0, -1.25), Object)
    assertEquals(purpose(igrins2Slit, 0,  2.4),  Object)

  test("IGRINS-2: exactly at the slit end is still Object"):
    assertEquals(purpose(igrins2Slit, 0,  2.5), Object)
    assertEquals(purpose(igrins2Slit, 0, -2.5), Object)

  test("IGRINS-2: past the slit end is Sky"):
    assertEquals(purpose(igrins2Slit, 0, 2.6),  Sky)
    assertEquals(purpose(igrins2Slit, 0, -2.6), Sky)

  test("IGRINS-2: across the slit width is Sky"):
    assertEquals(purpose(igrins2Slit, 0.1, 0), Object)
    assertEquals(purpose(igrins2Slit, 0.2, 0), Sky)

  test("IGRINS-2: the default nod-to-sky sky position is Sky"):
    assertEquals(purpose(igrins2Slit, 10, 10), Sky)

  // GNIRS low resolution IFU: 3.15" (p) by 4.8" (q)
  private val gnirsIfu: ShapeExpression =
    gnirs.scienceArea
      .shapeAt(Angle.Angle0, Offset.Zero, GnirsFpu.Spectroscopy.Ifu(GnirsFpuIfu.LowResolution), GnirsCamera.ShortBlue, GnirsPrism.Mirror)
      .get

  test("GNIRS IFU: small dithers are Object, the large offsets are Sky"):
    assertEquals(purpose(gnirsIfu,   0.15,   0.15),  Object)
    assertEquals(purpose(gnirsIfu,  -0.15,  -0.15),  Object)
    assertEquals(purpose(gnirsIfu,  10,     10),     Sky)
    assertEquals(purpose(gnirsIfu, -10,    -10),     Sky)

  test("object positions default to guided, sky positions to unguided"):
    assertEquals(Object.defaultGuiding, StepGuideState.Enabled)
    assertEquals(Sky.defaultGuiding,    StepGuideState.Disabled)
