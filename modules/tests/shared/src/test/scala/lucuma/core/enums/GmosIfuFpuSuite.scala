// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.all.*
import lucuma.core.util.Enumerated
import munit.FunSuite

/** `fromFpu` is the inverse of the widening each IFU aperture carries. */
class GmosIfuFpuSuite extends FunSuite:

  test("north: fromFpu round-trips every aperture"):
    Enumerated[GmosNorthIfuFpu].all.foreach: u =>
      assertEquals(GmosNorthIfuFpu.fromFpu(u.fpu), u.some)

  test("south: fromFpu round-trips every aperture"):
    Enumerated[GmosSouthIfuFpu].all.foreach: u =>
      assertEquals(GmosSouthIfuFpu.fromFpu(u.fpu), u.some)

  // The mode offers neither long slits nor the nod & shuffle IFUs.
  test("north: everything else is empty"):
    val ifu = Enumerated[GmosNorthIfuFpu].all.map(_.fpu).toSet
    Enumerated[GmosNorthFpu].all.filterNot(ifu).foreach: f =>
      assertEquals(GmosNorthIfuFpu.fromFpu(f), none, s"unexpected match for $f")

  test("south: everything else is empty, nod & shuffle IFUs included"):
    val ifu = Enumerated[GmosSouthIfuFpu].all.map(_.fpu).toSet
    Enumerated[GmosSouthFpu].all.filterNot(ifu).foreach: f =>
      assertEquals(GmosSouthIfuFpu.fromFpu(f), none, s"unexpected match for $f")
