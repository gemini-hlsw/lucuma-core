// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Coordinates
import lucuma.core.math.arb.ArbCoordinates.given
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbPosAngleConstraint.given
import lucuma.core.model.arb.ArbTarget.given
import lucuma.core.model.sequence.ghost.GhostIfuMappingSyntax.*
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbGid.given
import lucuma.core.util.arb.ArbTimestamp.given
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

final class GhostIfuMappingSuite extends ScalaCheckSuite:

  private def context(
    mode: GhostResolutionMode,
    sky:  Option[Coordinates],
    pac:  PosAngleConstraint,
    base: Option[Coordinates],
    when: Timestamp
  ): IfuMappingContext =
    IfuMappingContext(mode, sky, pac, base, when)

  property("opportunity target without a sky position maps to IFU1 alone"):
    forAll: (mode: GhostResolutionMode, pac: PosAngleConstraint, base: Option[Coordinates], when: Timestamp, tid: Target.Id, t: Target.Opportunity) =>
      val ctx = context(mode, none, pac, base, when)
      assertEquals(
        GhostIfuMapping.derive(ctx, List(tid -> t)),
        GhostIfuMapping.SingleTarget(tid).asRight[String]
      )

  property("opportunity target with a sky position maps to IFU1 plus sky"):
    forAll: (mode: GhostResolutionMode, sky: Coordinates, pac: PosAngleConstraint, base: Option[Coordinates], when: Timestamp, tid: Target.Id, t: Target.Opportunity) =>
      val ctx = context(mode, sky.some, pac, base, when)
      assertEquals(
        GhostIfuMapping.derive(ctx, List(tid -> t)),
        GhostIfuMapping.TargetPlusSky(tid, sky).asRight[String]
      )

  property("opportunity target validates"):
    forAll: (mode: GhostResolutionMode, sky: Option[Coordinates], pac: PosAngleConstraint, when: Timestamp, tid: Target.Id, t: Target.Opportunity) =>
      val ctx = context(mode, sky, pac, none, when)
      assertEquals(GhostIfuMapping.validate(ctx, List(tid -> t)), none)

  property("dual target mode does not accept opportunity targets"):
    forAll: (pac: PosAngleConstraint, when: Timestamp, tid1: Target.Id, t1: Target.Opportunity, tid2: Target.Id, t2: Target.Opportunity) =>
      val ctx = context(GhostResolutionMode.Standard, none, pac, none, when)
      assertEquals(
        GhostIfuMapping.derive(ctx, List(tid1 -> t1, tid2 -> t2)),
        "GHOST Dual Target mode is available for sidereal targets only.".asLeft[GhostIfuMapping]
      )
