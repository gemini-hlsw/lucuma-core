// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.syntax.option.*
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

/**
 * A user-specified telescope position, an entry in an observation's offset
 * pattern.  The purpose may be left unspecified, in which case it is derived
 * (typically from the instrument geometry) when the pattern is turned into
 * steps.
 */
case class TelescopeConfigSpec(
  offset:          Offset,
  guiding:         StepGuideState,
  explicitPurpose: Option[OffsetPurpose]
):

  /**
   * Resolves the purpose, using the explicit purpose if specified and otherwise
   * `defaultPurpose` applied to the offset.
   */
  def toTelescopeConfig(defaultPurpose: Offset => OffsetPurpose): TelescopeConfig =
    TelescopeConfig(offset, guiding, explicitPurpose.getOrElse(defaultPurpose(offset)))

  def toTelescopeConfigAlongSlit: TelescopeConfigAlongSlit =
    TelescopeConfigAlongSlit(offset.q, guiding, explicitPurpose)

object TelescopeConfigSpec:

  val Default: TelescopeConfigSpec =
    TelescopeConfigSpec(Offset.Zero, StepGuideState.Enabled, none)

  /** A spec at `offset` with the given guiding whose purpose is derived. */
  def apply(offset: Offset, guiding: StepGuideState): TelescopeConfigSpec =
    TelescopeConfigSpec(offset, guiding, none)

  /**
   * A spec at `offset` with an explicit `purpose`, guided as that purpose
   * defaults to (see `OffsetPurpose.defaultGuiding`).
   */
  def forPurpose(offset: Offset, purpose: OffsetPurpose): TelescopeConfigSpec =
    TelescopeConfigSpec(offset, purpose.defaultGuiding, purpose.some)

  /** An object position at `offset`, guided. */
  def objectAt(offset: Offset): TelescopeConfigSpec =
    forPurpose(offset, OffsetPurpose.Object)

  /** A sky position at `offset`, unguided. */
  def skyAt(offset: Offset): TelescopeConfigSpec =
    forPurpose(offset, OffsetPurpose.Sky)

  /** @group Optics */
  val offset: Lens[TelescopeConfigSpec, Offset] =
    Focus[TelescopeConfigSpec](_.offset)

  /** @group Optics */
  val guiding: Lens[TelescopeConfigSpec, StepGuideState] =
    Focus[TelescopeConfigSpec](_.guiding)

  /** @group Optics */
  val explicitPurpose: Lens[TelescopeConfigSpec, Option[OffsetPurpose]] =
    Focus[TelescopeConfigSpec](_.explicitPurpose)

  given Order[TelescopeConfigSpec] = Order.by(t => (t.offset, t.guiding, t.explicitPurpose))

  // Explicitly define ordering to avoid auto derivation
  given Ordering[TelescopeConfigSpec] = Order[TelescopeConfigSpec].toOrdering
