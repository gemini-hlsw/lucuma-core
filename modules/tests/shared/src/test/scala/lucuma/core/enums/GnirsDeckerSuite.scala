// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated
import munit.DisciplineSuite

final class GnirsDeckerSuite extends DisciplineSuite {

  test("long slit decker follows the camera") {
    assertEquals(GnirsDecker.forCameraAndPrism(GnirsCamera.ShortBlue, GnirsPrism.Mirror), GnirsDecker.ShortCamLongSlit)
    assertEquals(GnirsDecker.forCameraAndPrism(GnirsCamera.ShortRed, GnirsPrism.Mirror), GnirsDecker.ShortCamLongSlit)
    assertEquals(GnirsDecker.forCameraAndPrism(GnirsCamera.LongBlue, GnirsPrism.Mirror), GnirsDecker.LongCamLongSlit)
    assertEquals(GnirsDecker.forCameraAndPrism(GnirsCamera.LongRed, GnirsPrism.Mirror), GnirsDecker.LongCamLongSlit)
  }

  test("cross dispersed decker follows the prism, whatever the camera") {
    Enumerated[GnirsCamera].all.foreach: c =>
      assertEquals(GnirsDecker.forCameraAndPrism(c, GnirsPrism.Sxd), GnirsDecker.ShortCamCrossDispersed, c.tag)
      assertEquals(GnirsDecker.forCameraAndPrism(c, GnirsPrism.Lxd), GnirsDecker.LongCamCrossDispersed, c.tag)
  }

}
