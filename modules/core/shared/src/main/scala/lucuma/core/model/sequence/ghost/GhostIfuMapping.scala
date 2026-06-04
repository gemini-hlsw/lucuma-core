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

  /**
   * Only IFU1 is used for single target mode.
   */
  case class SingleTarget(
    ifu1: Target.Id
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.SingleTarget

  /**
   * IFU1 is a sidereal target, IFU2 is a sky position.
   */
  case class TargetPlusSky(
    ifu1: Target.Id,
    ifu2: Coordinates
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.TargetPlusSky

  /**
   * IFU1 is a sky position, IFU2 is a sidereal target.
   */
  case class SkyPlusTarget(
    ifu1: Coordinates,
    ifu2: Target.Id
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.SkyPlusTarget

  /**
   * IFU1 and IFU2 are sidereal targets.
   */
  case class DualTarget(
    ifu1: Target.Id,
    ifu2: Target.Id
  ) extends GhostIfuMapping derives Eq:
    override def mappingType: GhostIfuMappingType = GhostIfuMappingType.DualTarget

  given Eq[GhostIfuMapping] =
    Eq.instance:
      case (a: SingleTarget,  b: SingleTarget)  => a === b
      case (a: SkyPlusTarget, b: SkyPlusTarget) => a === b
      case (a: TargetPlusSky, b: TargetPlusSky) => a === b
      case (a: DualTarget,    b: DualTarget)    => a === b
      case _                                    => false

  val singleTarget: Prism[GhostIfuMapping, SingleTarget] =
    GenPrism[GhostIfuMapping, SingleTarget]

  val targetPlusSky: Prism[GhostIfuMapping, TargetPlusSky] =
    GenPrism[GhostIfuMapping, TargetPlusSky]

  val skyPlusTarget: Prism[GhostIfuMapping, SkyPlusTarget] =
    GenPrism[GhostIfuMapping, SkyPlusTarget]

  val dualTarget: Prism[GhostIfuMapping, DualTarget] =
    GenPrism[GhostIfuMapping, DualTarget]