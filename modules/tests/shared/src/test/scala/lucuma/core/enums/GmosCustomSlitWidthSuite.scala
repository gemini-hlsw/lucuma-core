// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.all.*
import lucuma.core.util.Enumerated
import munit.FunSuite

/** Each custom slit width is paired with the builtin long slit of the same width. */
class GmosCustomSlitWidthSuite extends FunSuite:

  test("north: the equivalent FPU is the long slit of the same width"):
    Enumerated[GmosCustomSlitWidth].all.foreach: w =>
      assertEquals(w.northFpu.fpuType, GmosFpuType.LongSlit, s"$w is not paired with a long slit")
      assertEquals(w.northFpu.effectiveSlitWidth, w.width, s"$w is paired with a slit of a different width")

  test("south: the equivalent FPU is the long slit of the same width"):
    Enumerated[GmosCustomSlitWidth].all.foreach: w =>
      assertEquals(w.southFpu.fpuType, GmosFpuType.LongSlit, s"$w is not paired with a long slit")
      assertEquals(w.southFpu.effectiveSlitWidth, w.width, s"$w is paired with a slit of a different width")

  test("every builtin long slit has a custom slit width"):
    val north = Enumerated[GmosCustomSlitWidth].all.map(_.northFpu).toSet
    val south = Enumerated[GmosCustomSlitWidth].all.map(_.southFpu).toSet
    assertEquals(north, Enumerated[GmosNorthFpu].all.filter(_.fpuType === GmosFpuType.LongSlit).toSet)
    assertEquals(south, Enumerated[GmosSouthFpu].all.filter(_.fpuType === GmosFpuType.LongSlit).toSet)
