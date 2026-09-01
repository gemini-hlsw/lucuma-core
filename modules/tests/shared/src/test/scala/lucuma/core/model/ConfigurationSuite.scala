// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.syntax.all.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.FacilityObservingModeType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.arb.ArbConfiguration.given
import lucuma.core.util.arb.ArbEnumerated.given
import munit.ScalaCheckSuite
import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.rng.Seed

import scala.compiletime.constValueTuple
import scala.compiletime.summonAll
import scala.deriving.Mirror

final class ConfigurationSuite extends ScalaCheckSuite:
  import Configuration.ObservingMode.*
  import Configuration.ObservingMode.Radii

  // Guards against adding an ObservingModeType without a Configuration.ObservingMode for it.  The
  // subclasses come from the compiler-derived Mirror, so there is no list here to keep up to date;
  // a new subclass without an Arbitrary given fails to compile.  Exchange modes are deliberately
  // excluded: they are not Gemini instruments and need no configuration approval.
  test("every facility and visitor ObservingModeType has exactly one Configuration.ObservingMode"):
    val m      = summon[Mirror.SumOf[Configuration.ObservingMode]]
    val labels = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val arbs   = summonAll[Tuple.Map[m.MirroredElemTypes, Arbitrary]].toList.asInstanceOf[List[Arbitrary[Configuration.ObservingMode]]]

    // Enough samples, with a fixed seed, to reach every mode a subclass can produce (e.g. Visitor).
    val produced: Map[ObservingModeType, Set[String]] =
      labels.zip(arbs).flatMap { (label, arb) =>
        Gen.listOfN(500, arb.arbitrary)(Gen.Parameters.default, Seed(0L)).orEmpty.map(_.tpe -> label)
      }.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

    val expected: Set[ObservingModeType] =
      (FacilityObservingModeType.values ++ VisitorObservingModeType.values).toSet

    val missing    = (expected -- produced.keySet).toList.map(t => s"No Configuration.ObservingMode for ${t.tag}")
    val unexpected = (produced.keySet -- expected).toList.map(t => s"Unexpected Configuration.ObservingMode for ${t.tag}")
    val duplicated = produced.toList.collect:
      case (t, ls) if ls.sizeIs > 1 => s"More than one Configuration.ObservingMode for ${t.tag}: ${ls.toList.sorted.mkString(", ")}"

    val problems = missing ++ unexpected ++ duplicated
    assert(problems.isEmpty, problems.mkString("\n"))

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

  test("an approved scheduling mode covers itself and every looser rung"):
    forAll: (cfg: Configuration, a: SchedulingMode, b: SchedulingMode) =>
      val ca = cfg.copy(schedulingMode = a)
      val cb = cfg.copy(schedulingMode = b)
      assertEquals(ca.subsumes(cb), cfg.subsumes(cfg) && a >= b)

  test("approval of Uninterruptible covers an Unconstrained observation, not the reverse"):
    forAll: (cfg: Configuration) =>
      val strict = cfg.copy(schedulingMode = SchedulingMode.Uninterruptible)
      val loose  = cfg.copy(schedulingMode = SchedulingMode.Unconstrained)
      assertEquals(strict.subsumes(loose), cfg.subsumes(cfg))
      assert(!loose.subsumes(strict))

  test("Flamingos2 MOS is constrained by disperser"):
    forAll: (cfg: Configuration, a: Flamingos2Mos, b: Flamingos2Mos) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assertEquals(ca.subsumes(cb), a.disperser === b.disperser)
      assertEquals(cb.subsumes(ca), a.disperser === b.disperser)
