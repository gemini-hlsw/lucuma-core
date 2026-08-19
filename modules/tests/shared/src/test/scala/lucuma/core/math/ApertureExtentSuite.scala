// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Show
import cats.kernel.laws.discipline.*
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.math.arb.*
import lucuma.core.math.syntax.int.*
import monocle.law.discipline.*
import org.scalacheck.Prop.*

final class ApertureExtentSuite extends munit.DisciplineSuite {
  import ArbAngle.given
  import ArbApertureExtent.given
  import ArbOffset.given

  checkAll("ApertureExtent", OrderTests[ApertureExtent].order)
  checkAll("ApertureExtent.p", LensTests(ApertureExtent.p))
  checkAll("ApertureExtent.q", LensTests(ApertureExtent.q))

  test("Show must be natural") {
    forAll { (a: ApertureExtent) =>
      assertEquals(a.toString, Show[ApertureExtent].show(a))
    }
  }

  test("the origin is contained by any non-degenerate extent") {
    forAll { (a: ApertureExtent) =>
      assertEquals(
        a.contains(Offset.Zero),
        a.p.toMicroarcseconds > 0 && a.q.toMicroarcseconds > 0
      )
    }
  }

  test("containment is symmetric about the origin") {
    forAll { (a: ApertureExtent, o: Offset) =>
      assertEquals(a.contains(o), a.contains(-o))
    }
  }

  test("swap exchanges the axes of the containment test") {
    forAll { (a: ApertureExtent, o: Offset) =>
      assertEquals(a.contains(o), a.swap.contains(Offset(o.q.toAngle.p, o.p.toAngle.q)))
    }
  }

  test("the edge is not contained") {
    val a = ApertureExtent(1.arcsec, 2.arcsec)
    assert(a.contains(Offset(400.mas.p, 900.mas.q)))
    assert(!a.contains(Offset(500.mas.p, 0.mas.q)))
    assert(!a.contains(Offset(0.mas.p, 1000.mas.q)))
    assert(!a.contains(Offset(600.mas.p, 0.mas.q)))
  }

  test("GMOS long slits are the slit width by 330.4 arcsec") {
    assertEquals(
      GmosNorthFpu.LongSlit_0_50.apertureExtent,
      Some(ApertureExtent(500.mas, 330400.mas))
    )
    assertEquals(
      GmosSouthFpu.LongSlit_5_00.apertureExtent,
      Some(ApertureExtent(5000.mas, 330400.mas))
    )
  }

  test("GMOS nod & shuffle slits are the slit width by 108 arcsec") {
    assertEquals(
      GmosNorthFpu.Ns3.apertureExtent,
      Some(ApertureExtent(1000.mas, 108000.mas))
    )
  }

  test("GMOS IFUs have no extent, since the field size is not the pseudo-slit width") {
    assertEquals(GmosNorthFpu.Ifu2Slits.apertureExtent, None)
    assertEquals(GmosSouthFpu.IfuNSBlue.apertureExtent, None)
  }

  test("a nod inside a GMOS long slit is on source but a sky nod is not") {
    val slit = GmosNorthFpu.LongSlit_1_00.apertureExtent.get
    assert(slit.contains(Offset(0.mas.p, 15.arcsec.q)))
    assert(!slit.contains(Offset(0.mas.p, 200.arcsec.q)))
    assert(!slit.contains(Offset(2.arcsec.p, 0.mas.q)))
  }

  test("GNIRS slit length depends on camera and prism") {
    val slit = GnirsFpuSlit.LongSlit_0_30
    assertEquals(slit.apertureExtent(GnirsCamera.ShortBlue, GnirsPrism.Mirror).q, 99.arcsec)
    assertEquals(slit.apertureExtent(GnirsCamera.LongBlue, GnirsPrism.Mirror).q, 49.arcsec)
    assertEquals(slit.apertureExtent(GnirsCamera.LongBlue, GnirsPrism.Lxd).q, 5100.mas)
  }

  test("the GNIRS LR-IFU contains its own default point-source dither") {
    // LowResolutionIfuPresets' "Point" preset dithers to p = ±0.75", far wider
    // than the widest GNIRS slit; only a field-shaped aperture accepts it.
    val ifu = GnirsFpuIfu.LowResolution.apertureExtent
    assert(ifu.contains(Offset(750.mas.p, 1500.mas.q)))
    assert(ifu.contains(Offset(-750.mas.p, -1500.mas.q)))
    assert(!ifu.contains(Offset(2.arcsec.p, 0.mas.q)))
  }

  test("Flamingos 2 converts its pixel slit width to an angle") {
    assertEquals(Flamingos2Fpu.LongSlit1.slitWidthAngle, 180.mas)
    assertEquals(Flamingos2Fpu.LongSlit3.slitWidthAngle, 540.mas)
    assertEquals(
      Flamingos2Fpu.LongSlit2.apertureExtent,
      Some(ApertureExtent(360.mas, 263.arcsec))
    )
    assertEquals(Flamingos2Fpu.Pinhole.apertureExtent, None)
  }
}
