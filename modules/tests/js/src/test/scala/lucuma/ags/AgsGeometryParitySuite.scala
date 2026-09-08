// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.geom.Area
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.JtsShapeInterpreter
import lucuma.core.geom.syntax.all.*
import lucuma.core.geom.visitors.MaroonXScienceFov
import lucuma.core.geom.visitors.MaroonXSkyFiberPatrol
import lucuma.core.geom.wasm.WasmKernelSuite
import lucuma.core.geom.wasm.WasmShapeInterpreter
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

/**
 * Cross-engine parity: every `AgsParams` variant evaluated with JTS and with the wasm kernel,
 * exercising the operations AGS performs on the results (`SingleProbeAgsParams.posCalculations`):
 * patrol-field reachability, probe-arm vignetting area and protected-area overlap.
 *
 * Tolerances: area within 1e-7 relative, bounding boxes and radii within 10 µas, `contains` and
 * `intersects` exact.
 */
class AgsGeometryParitySuite extends munit.FunSuite with WasmKernelSuite:

  private val AreaRel  = 1e-7
  private val AngleTol = 10L
  private val GridN    = 11

  // The kernel's overlay (i_overlay via geo) snaps vertices to a grid of 2^29 steps across the
  // operands' combined extent, so an overlay result carries an area error of about
  // perimeter x step on top of the 1e-7 budget; primitives and predicates are unaffected.
  private val OverlayGridSteps = math.pow(2, 29)

  private type Params = AgsParams & SingleProbeAgsParams

  private def off(p: Double, q: Double): Offset =
    Offset(Offset.P(Angle.fromDoubleArcseconds(p)), Offset.Q(Angle.fromDoubleArcseconds(q)))

  private val posAngles: List[Angle] =
    List(0.0, 37.5, 90.0, 145.0, 210.0, 300.0).map(Angle.fromDoubleDegrees)

  private val offsets: List[Offset] =
    List(Offset.Zero, off(10, -5), off(-45.25, 30.5))

  private val pivot: Offset = off(7, 3)

  // A 20-offset dither pattern, as `posCalculations` intersects for a sequence.
  private val ditherOffsets: List[Offset] =
    (0 until 20).toList.map(k => off(k * 1.7 - 17, (k * 2.3) % 11 - 5))

  private def withProbes[A <: Params & PwfsSupport[A]](name: String, a: A): List[(String, Params)] =
    val pwfs2 = a.probe match
      case GuideProbe.PWFS2 => Nil
      case _                => List(s"$name PWFS2" -> a.withPWFS2)
    List(name -> a, s"$name PWFS1" -> a.withPWFS1) ++ pwfs2

  private val variants: List[(String, Params)] =
    withProbes("GMOS imaging side", AgsParams.GmosImaging(PortDisposition.Side)) ++
      withProbes("GMOS imaging bottom", AgsParams.GmosImaging(PortDisposition.Bottom)) ++
      withProbes("GMOS-N long slit 1.0", AgsParams.GmosLongSlit(GmosNorthFpu.LongSlit_1_00.asLeft)) ++
      withProbes(
        "GMOS-S long slit 0.5 bottom",
        AgsParams.GmosLongSlit(GmosSouthFpu.LongSlit_0_50.asRight, PortDisposition.Bottom)
      ) ++
      withProbes("GMOS-N N&S 0.5", AgsParams.GmosLongSlit(GmosNorthFpu.Ns1.asLeft)) ++
      withProbes("GMOS-N IFU-2", AgsParams.GmosIfu(GmosNorthIfuFpu.TwoSlits.asLeft)) ++
      withProbes(
        "GMOS-S IFU-R bottom",
        AgsParams.GmosIfu(GmosSouthIfuFpu.OneSlitRed.asRight, PortDisposition.Bottom)
      ) ++
      withProbes("GMOS-N MOS", AgsParams.GmosMos(Site.GN)) ++
      withProbes("GMOS-S MOS bottom", AgsParams.GmosMos(Site.GS, PortDisposition.Bottom)) ++
      withProbes(
        "F2 imaging f/16 side",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.F16, PortDisposition.Side)
      ) ++
      withProbes(
        "F2 imaging GeMS-under bottom",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.GemsUnder, PortDisposition.Bottom)
      ) ++
      withProbes(
        "F2 imaging GeMS-under side",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.GemsUnder, PortDisposition.Side)
      ) ++
      withProbes(
        "F2 imaging GeMS-over bottom",
        AgsParams.Flamingos2Imaging(Flamingos2LyotWheel.GemsOver, PortDisposition.Bottom)
      ) ++
      withProbes(
        "F2 long slit 2px",
        AgsParams.Flamingos2LongSlit(
          Flamingos2LyotWheel.F16,
          Flamingos2FpuMask.Builtin(Flamingos2Fpu.LongSlit2),
          PortDisposition.Side
        )
      ) ++
      withProbes("F2 MOS", AgsParams.Flamingos2Mos(Flamingos2LyotWheel.F16, PortDisposition.Side)) ++
      withProbes("IGRINS-2", AgsParams.Igrins2LongSlit()) ++
      withProbes(
        "GNIRS long slit",
        AgsParams.GnirsLongSlit(GnirsFpuSlit.LongSlit_0_30, GnirsCamera.ShortBlue, GnirsPrism.Mirror)
      ) ++
      withProbes("GNIRS imaging keyhole", AgsParams.GnirsImaging(GnirsCamera.LongRed, GnirsFilter.Order4)) ++
      withProbes("GNIRS IFU", AgsParams.GnirsIfu(GnirsFpuIfu.LowResolution)) ++
      withProbes("GHOST", AgsParams.GhostIfu()) ++
      withProbes("MaroonX", AgsParams.Visitor(MaroonXSkyFiberPatrol, MaroonXScienceFov)) ++
      withProbes("Visitor 30/10", AgsParams.Visitor(30.arcsec, 10.arcsec))

  private def jts(e: ShapeExpression): Shape  = JtsShapeInterpreter.interpret(e)
  private def wasm(e: ShapeExpression): Shape = WasmShapeInterpreter.interpret(e)

  private def µas(a: Angle): Long = Angle.signedMicroarcseconds.get(a)

  private def assertArea(w: Area, j: Area, clue: String): Unit =
    val a   = w.toMicroarcsecondsSquared.toDouble
    val b   = j.toMicroarcsecondsSquared.toDouble
    val tol = math.max(b.abs * AreaRel, 1.0)
    assert((a - b).abs <= tol, s"$clue: area $a vs JTS $b (tol $tol)")

  private def extent(b: BoundingOffsets): Double = b.maxSide.toMicroarcseconds.toDouble

  private def perimeter(b: BoundingOffsets): Double =
    2.0 * (b.topLeft.p.toAngle.difference(b.bottomRight.p.toAngle).toMicroarcseconds +
      b.topLeft.q.toAngle.difference(b.bottomRight.q.toAngle).toMicroarcseconds)

  /** Overlay result `wi`/`ji` of operands whose combined extent is `ext` µas. */
  private def assertOverlayArea(wi: Shape, ji: Shape, ext: Double, clue: String): Unit =
    val a    = wi.area.toMicroarcsecondsSquared.toDouble
    val b    = ji.area.toMicroarcsecondsSquared.toDouble
    val snap = if isEmpty(ji) then 0.0 else 2.0 * (ext / OverlayGridSteps) * perimeter(ji.boundingOffsets)
    val tol  = math.max(math.max(b.abs * AreaRel, snap), 1.0)
    assert((a - b).abs <= tol, s"$clue: area $a vs JTS $b (tol $tol, snap $snap)")

  private def attempt[A](clue: String)(a: => A): A =
    try a
    catch case t: Throwable => fail(s"$clue threw ${t.getClass.getSimpleName}: ${t.getMessage}")

  // JTS robustness failures found while evaluating AGS geometry; the kernel must still evaluate
  // them and the list is pinned below so a change in either engine shows up.
  private val jtsTopologyFailures = scala.collection.mutable.Map.empty[String, Int]

  private def jtsOrTopologyFailure(variant: String)(s: => Shape): Option[Shape] =
    try Some(s)
    catch
      case t: org.locationtech.jts.geom.TopologyException =>
        jtsTopologyFailures.updateWith(variant)(n => Some(n.getOrElse(0) + 1))
        None

  private def assertAngle(w: Angle, j: Angle, clue: String): Unit =
    val d = µas(w - j).abs
    assert(d <= AngleTol, s"$clue: $d µas apart (kernel $w, JTS $j)")

  private def assertBounds(w: BoundingOffsets, j: BoundingOffsets, clue: String): Unit =
    assertAngle(w.topLeft.p.toAngle, j.topLeft.p.toAngle, s"$clue top-left p")
    assertAngle(w.topLeft.q.toAngle, j.topLeft.q.toAngle, s"$clue top-left q")
    assertAngle(w.bottomRight.p.toAngle, j.bottomRight.p.toAngle, s"$clue bottom-right p")
    assertAngle(w.bottomRight.q.toAngle, j.bottomRight.q.toAngle, s"$clue bottom-right q")

  // Points over the bounding box plus a 10% margin; the fractional phase keeps them off the
  // lattice edges and vertices land on.
  private def probePoints(b: BoundingOffsets): List[Offset] =
    val pMin = µas(b.bottomRight.p.toAngle)
    val qMin = µas(b.bottomRight.q.toAngle)
    val pw   = math.max(µas(b.topLeft.p.toAngle) - pMin, 2_000_000L).toDouble
    val qw   = math.max(µas(b.topLeft.q.toAngle) - qMin, 2_000_000L).toDouble
    for
      i <- (0 until GridN).toList
      k <- (0 until GridN).toList
    yield Offset(
      Offset.P(Angle.fromMicroarcseconds(pMin + (pw * (-0.1 + 1.2 * (i + 0.37) / GridN)).round)),
      Offset.Q(Angle.fromMicroarcseconds(qMin + (qw * (-0.1 + 1.2 * (k + 0.61) / GridN)).round))
    )

  // Candidate guide stars: the centre and inner quartiles of a bounding box.
  private def candidates(b: BoundingOffsets): List[Offset] =
    val pMin = µas(b.bottomRight.p.toAngle)
    val qMin = µas(b.bottomRight.q.toAngle)
    val pw   = (µas(b.topLeft.p.toAngle) - pMin).toDouble
    val qw   = (µas(b.topLeft.q.toAngle) - qMin).toDouble
    List((0.5, 0.5), (0.25, 0.25), (0.75, 0.25), (0.25, 0.75), (0.75, 0.75)).map: (fp, fq) =>
      Offset(
        Offset.P(Angle.fromMicroarcseconds(pMin + (pw * fp).round)),
        Offset.Q(Angle.fromMicroarcseconds(qMin + (qw * fq).round))
      )

  private def isEmpty(s: Shape): Boolean = s.area.toMicroarcsecondsSquared === 0L

  /** Full parity on one expression; returns both shapes for further checks. */
  private def assertParity(e: ShapeExpression, clue: String): (Shape, Shape) =
    val w = attempt(s"$clue kernel")(wasm(e))
    val j = attempt(s"$clue JTS")(jts(e))
    assertArea(w.area, j.area, clue)
    assertAngle(w.radius, j.radius, s"$clue radius")
    // JTS reports a null envelope for an empty geometry; the kernel a zero box.
    if !isEmpty(j) then
      assertBounds(w.boundingOffsets, j.boundingOffsets, s"$clue bbox")
      probePoints(j.boundingOffsets).foreach: o =>
        assertEquals(w.contains(o), j.contains(o), s"$clue contains $o")
    (w, j)

  private def protectedAreas(params: Params): List[(ShapeExpression, Offset)] =
    val d = params.scienceDiameter
    offsets.map(nz => (ShapeExpression.centeredEllipse(d, d) ↗ nz, nz))

  private def overlaps(arm: Shape, protectedShape: Shape): Boolean =
    arm.intersection(protectedShape).boundingOffsets.maxSide.toMicroarcseconds > 5

  private def assertAgsOps(name: String, params: Params, pa: Angle, offset: Offset): Unit =
    val clue      = s"$name PA ${pa.toDoubleDegrees} offset $offset"
    val (wPf, jPf) = assertParity(params.patrolFieldAt(pa, offset), s"$clue patrol field")
    val (wSa, jSa) = assertParity(params.scienceArea(pa, offset), s"$clue science area")
    val vignetting = params.extendedVignettingArea.fold((wSa, jSa)): f =>
      assertParity(f(pa, offset), s"$clue extended vignetting area")
    val (wVig, jVig) = vignetting
    val prot       = protectedAreas(params).map((e, nz) => (wasm(e), jts(e), nz))
    candidates(jPf.boundingOffsets).foreach: gs =>
      val gsClue = s"$clue guide star $gs"
      assertEquals(wPf.contains(gs), jPf.contains(gs), s"$gsClue reachable")
      val armExpr = params.probeArm(pa, gs, offset)
      jtsOrTopologyFailure(name)(jts(armExpr)) match
        case None    =>
          val wArm = attempt(s"$gsClue kernel")(wasm(armExpr))
          assert(!isEmpty(wArm), s"$gsClue: kernel probe arm is empty where JTS failed")
        case Some(_) => assertArmOps(gsClue, armExpr, wVig, jVig, prot)

  private def assertArmOps(
    gsClue:  String,
    armExpr: ShapeExpression,
    wVig:    Shape,
    jVig:    Shape,
    prot:    List[(Shape, Shape, Offset)]
  ): Unit =
    val (wArm, jArm) = assertParity(armExpr, s"$gsClue probe arm")
    val ext          = math.max(extent(jArm.boundingOffsets), extent(jVig.boundingOffsets))
    assertEquals(wArm.intersects(wVig), jArm.intersects(jVig), s"$gsClue intersects vignetting area")
    val wV = attempt(s"$gsClue kernel vignetting")(wArm.intersection(wVig))
    val jV = attempt(s"$gsClue JTS vignetting")(jArm.intersection(jVig))
    assertOverlayArea(wV, jV, ext, s"$gsClue vignetting")
    prot.foreach: (wP, jP, nz) =>
      val wI = attempt(s"$gsClue kernel protected overlap at $nz")(wArm.intersection(wP))
      val jI = attempt(s"$gsClue JTS protected overlap at $nz")(jArm.intersection(jP))
      assertEquals(overlaps(wArm, wP), overlaps(jArm, jP), s"$gsClue overlaps protected area at $nz")
      if !isEmpty(jI) then
        assertAngle(wI.boundingOffsets.maxSide, jI.boundingOffsets.maxSide, s"$gsClue protected overlap max side at $nz")

  variants.foreach: (name, params) =>
    test(s"$name: AGS geometry matches JTS at every PA and offset"):
      for
        pa     <- posAngles
        offset <- offsets
      do assertAgsOps(name, params, pa, offset)

    test(s"$name: patrol field around a pivot matches JTS"):
      posAngles.foreach: pa =>
        assertParity(params.patrolFieldAt(pa, offsets(1), pivot), s"$name PA ${pa.toDoubleDegrees} pivot")

    test(s"$name: 20-offset patrol field intersection matches JTS"):
      List(posAngles(0), posAngles(3)).foreach: pa =>
        val chain = ditherOffsets.map(o => params.patrolFieldAt(pa, o)).reduce(_ ∩ _)
        val (w, j) = assertParity(chain, s"$name PA ${pa.toDoubleDegrees} dither chain")
        assertParity(chain.boundingBox, s"$name PA ${pa.toDoubleDegrees} dither chain bbox")
        assertEquals(isEmpty(w), isEmpty(j), s"$name PA ${pa.toDoubleDegrees} dither chain emptiness")

  // lucuma-jts (OverlayNG union, "side location conflict") cannot build the F2 OIWFS probe arm
  // (arm ∪ pickoff mirror) at the GeMS plate scale, on either port, for any PA, offset or guide
  // star tried; F/16 is fine and the kernel evaluates every one of them. Counts are probe-arm
  // evaluations (6 PAs x 3 offsets x 5 guide stars).
  test("JTS topology failures are exactly the known ones (the kernel evaluates all of them)"):
    assertEquals(
      jtsTopologyFailures.toMap,
      Map(
        "F2 imaging GeMS-under bottom" -> 90,
        "F2 imaging GeMS-under side"   -> 90,
        "F2 imaging GeMS-over bottom"  -> 90
      )
    )
