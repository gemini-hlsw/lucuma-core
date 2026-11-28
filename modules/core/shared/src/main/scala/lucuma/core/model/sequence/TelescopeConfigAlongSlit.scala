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
 * A user-specified telescope position along the slit (offset in q only).  As for
 * `TelescopeConfigSpec`, the purpose may be left unspecified and derived later.
 *
 * The default along-slit patterns leave it unspecified: their positions are all
 * on the slit, so they resolve to `Object` anyway, and the ODB duplicates them
 * in its views.
 */
case class TelescopeConfigAlongSlit(
  offset:          Offset.Q,
  guiding:         StepGuideState,
  explicitPurpose: Option[OffsetPurpose]
):
  def toTelescopeConfigSpec: TelescopeConfigSpec =
    TelescopeConfigSpec(Offset(Offset.P.Zero, offset), guiding, explicitPurpose)

  /**
   * Resolves the purpose, using the explicit purpose if specified and otherwise
   * `defaultPurpose` applied to the offset.
   */
  def toTelescopeConfig(defaultPurpose: Offset => OffsetPurpose): TelescopeConfig =
    toTelescopeConfigSpec.toTelescopeConfig(defaultPurpose)

object TelescopeConfigAlongSlit:

  val Default: TelescopeConfigAlongSlit =
    TelescopeConfigAlongSlit(Offset.Q.Zero, StepGuideState.Enabled, none)

  /** An along-slit position with the given guiding whose purpose is derived. */
  def apply(offset: Offset.Q, guiding: StepGuideState): TelescopeConfigAlongSlit =
    TelescopeConfigAlongSlit(offset, guiding, none)

  /**
   * An along-slit position with an explicit `purpose`, guided as that purpose
   * defaults to (see `OffsetPurpose.defaultGuiding`).
   */
  def forPurpose(offset: Offset.Q, purpose: OffsetPurpose): TelescopeConfigAlongSlit =
    TelescopeConfigAlongSlit(offset, purpose.defaultGuiding, purpose.some)

  /** An object position at `offset` along the slit, guided. */
  def objectAt(offset: Offset.Q): TelescopeConfigAlongSlit =
    forPurpose(offset, OffsetPurpose.Object)

  /** A sky position at `offset` along the slit, unguided. */
  def skyAt(offset: Offset.Q): TelescopeConfigAlongSlit =
    forPurpose(offset, OffsetPurpose.Sky)

  /** @group Optics */
  val offset: Lens[TelescopeConfigAlongSlit, Offset.Q] =
    Focus[TelescopeConfigAlongSlit](_.offset)

  /** @group Optics */
  val guiding: Lens[TelescopeConfigAlongSlit, StepGuideState] =
    Focus[TelescopeConfigAlongSlit](_.guiding)

  /** @group Optics */
  val explicitPurpose: Lens[TelescopeConfigAlongSlit, Option[OffsetPurpose]] =
    Focus[TelescopeConfigAlongSlit](_.explicitPurpose)

  given Order[TelescopeConfigAlongSlit] = Order.by(t => (t.offset, t.guiding, t.explicitPurpose))
