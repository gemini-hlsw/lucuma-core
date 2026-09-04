// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated
import munit.DisciplineSuite

final class GnirsCameraSuite extends DisciplineSuite {

  test("blue and red keep the pixel scale") {
    Enumerated[GnirsCamera].all.foreach: c =>
      assertEquals(c.blue.pixelScale, c.pixelScale, s"${c.tag}.blue changed the pixel scale")
      assertEquals(c.red.pixelScale, c.pixelScale, s"${c.tag}.red changed the pixel scale")
  }

  test("blue and red are idempotent and mutually inverse") {
    Enumerated[GnirsCamera].all.foreach: c =>
      assertEquals(c.blue.blue, c.blue)
      assertEquals(c.red.red, c.red)
      assertEquals(c.blue.red, c.red)
      assertEquals(c.red.blue, c.blue)
  }

  test("the pairs") {
    assertEquals(GnirsCamera.ShortRed.blue, GnirsCamera.ShortBlue)
    assertEquals(GnirsCamera.LongRed.blue, GnirsCamera.LongBlue)
    assertEquals(GnirsCamera.ShortBlue.blue, GnirsCamera.ShortBlue)
    assertEquals(GnirsCamera.LongBlue.red, GnirsCamera.LongRed)
  }

}
