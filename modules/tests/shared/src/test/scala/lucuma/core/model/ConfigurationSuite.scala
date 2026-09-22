// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsPrism
import lucuma.core.model.arb.ArbConfiguration.given
import lucuma.core.util.arb.ArbEnumerated.given
import munit.ScalaCheckSuite
import org.scalacheck.*
import org.scalacheck.Prop.*

final class ConfigurationSuite extends ScalaCheckSuite:
  import Configuration.ObservingMode.*
  import Configuration.ObservingMode.Radii

  test("Flamingos2 Imaging has no constraints"):
    forAll: (cfg: Configuration) =>
      val c = cfg.copy(observingMode = Flamingos2Imaging)
      assert(c.subsumes(c))

  test("GNIRS Imaging has no constraints"):
    forAll: (cfg: Configuration) =>
      val c = cfg.copy(observingMode = GnirsImaging)
      assert(c.subsumes(c))

  // The approval radius is the round field the MK filters see, the smallest GNIRS imaging
  // science area, so that a base moved anywhere within it stays on the detector whatever
  // filter is used. The keyhole the other filters see is bounded below by the no-XD slit
  // length, which is how the comparison below pins that the smaller field was chosen.
  test("GNIRS Imaging radius is the round field, smaller than the keyhole"):
    assertEqualsDouble(Radii.GnirsImaging.toSignedDoubleDegrees * 3600, 12.816, 0.001)
    forAll: (camera: GnirsCamera) =>
      assert(Radii.GnirsImaging.toMicroarcseconds < Radii.gnirsLongSlit(camera, GnirsPrism.Mirror).toMicroarcseconds)

  test("GMOS North Imaging has no constraints"):
    forAll: (cfg: Configuration, a: GmosNorthImaging, b: GmosNorthImaging) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assert(ca.subsumes(cb))
      assert(cb.subsumes(ca))

  test("GMOS South Imaging has no constraints"):
    forAll: (cfg: Configuration, a: GmosSouthImaging, b: GmosSouthImaging) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assert(ca.subsumes(cb))
      assert(cb.subsumes(ca))

  test("GMOS-N MOS is constrained by grating"):
    forAll: (cfg: Configuration, a: GmosNorthMos, b: GmosNorthMos) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assertEquals(ca.subsumes(cb), a.grating === b.grating)
      assertEquals(cb.subsumes(ca), a.grating === b.grating)

  test("GMOS-S MOS is constrained by grating"):
    forAll: (cfg: Configuration, a: GmosSouthMos, b: GmosSouthMos) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assertEquals(ca.subsumes(cb), a.grating === b.grating)
      assertEquals(cb.subsumes(ca), a.grating === b.grating)

  test("Altair mode must match exactly"):
    forAll: (cfg: Configuration, a: Option[AltairMode], b: Option[AltairMode]) =>
      val ca = cfg.copy(altair = a)
      val cb = cfg.copy(altair = b)
      assertEquals(ca.subsumes(cb), a === b)
      assertEquals(cb.subsumes(ca), a === b)

  test("Flamingos2 MOS is constrained by disperser"):
    forAll: (cfg: Configuration, a: Flamingos2Mos, b: Flamingos2Mos) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assertEquals(ca.subsumes(cb), a.disperser === b.disperser)
      assertEquals(cb.subsumes(ca), a.disperser === b.disperser)
