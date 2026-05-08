// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.ghost

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import lucuma.core.enums.GhostIfuMappingType
import lucuma.core.math.Coordinates
import lucuma.core.model.Target

import monocle.Prism
import monocle.macros.GenPrism

sealed trait GhostIfuMapping:
  def mappingType: GhostIfuMappingType

object GhostIfuMapping:

  case class GhostSingleTarget(
    ifu1: Target
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.SingleTarget

  case class GhostTargetPlusSky(
    ifu1: Target,
    ifu2: Coordinates
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.TargetPlusSky

  case class GhostSkyPlusTarget(
    ifu1: Coordinates,
    ifu2: Target
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.SkyPlusTarget

  case class GhostDualTarget(
    ifu1: Target,
    ifu2: Target
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.DualTarget

  given Eq[GhostIfuMapping] =
    Eq.instance:
      case (a: GhostSingleTarget,  b: GhostSingleTarget)  => a === b
      case (a: GhostSkyPlusTarget, b: GhostSkyPlusTarget) => a === b
      case (a: GhostTargetPlusSky, b: GhostTargetPlusSky) => a === b
      case (a: GhostDualTarget,    b: GhostDualTarget)    => a === b
      case _                                              => false

  val singleTarget: Prism[GhostIfuMapping, GhostSingleTarget] =
    GenPrism[GhostIfuMapping, GhostSingleTarget]

  val targetPlusSky: Prism[GhostIfuMapping, GhostTargetPlusSky] =
    GenPrism[GhostIfuMapping, GhostTargetPlusSky]

  val skyPlusTarget: Prism[GhostIfuMapping, GhostSkyPlusTarget] =
    GenPrism[GhostIfuMapping, GhostSkyPlusTarget]

  val dualTarget: Prism[GhostIfuMapping, GhostDualTarget] =
    GenPrism[GhostIfuMapping, GhostDualTarget]