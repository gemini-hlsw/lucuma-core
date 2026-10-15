// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import cats.derived.*
import cats.kernel.CommutativeMonoid
import cats.syntax.monoid.*
import lucuma.core.enums.SetupKind
import monocle.Focus
import monocle.Lens

/**
 * The setups executed for an observation (or a collection of observations),
 * with the time charged for them, split by kind.  The counts and times
 * correspond to the setup and reacquisition estimates in the
 * `ExecutionDigest`.
 */
case class SetupCharge(
  full:          SetupChargeSummary,
  reacquisition: SetupChargeSummary
) derives Order:

  /** Summary for the given kind of setup. */
  def apply(kind: SetupKind): SetupChargeSummary =
    kind match
      case SetupKind.Full          => full
      case SetupKind.Reacquisition => reacquisition

  /** Total time charged for all setups, full and reacquisition. */
  def total: SetupTimeBreakdown =
    full.time |+| reacquisition.time

object SetupCharge:

  val Zero: SetupCharge =
    SetupCharge(SetupChargeSummary.Zero, SetupChargeSummary.Zero)

  /** A setup charge with the given summary for one kind and zero otherwise. */
  def of(kind: SetupKind, summary: SetupChargeSummary): SetupCharge =
    kind match
      case SetupKind.Full          => Zero.copy(full = summary)
      case SetupKind.Reacquisition => Zero.copy(reacquisition = summary)

  given CommutativeMonoid[SetupCharge] =
    CommutativeMonoid.instance(
      Zero,
      (a, b) => SetupCharge(a.full |+| b.full, a.reacquisition |+| b.reacquisition)
    )

  /** @group Optics */
  val full: Lens[SetupCharge, SetupChargeSummary] =
    Focus[SetupCharge](_.full)

  /** @group Optics */
  val reacquisition: Lens[SetupCharge, SetupChargeSummary] =
    Focus[SetupCharge](_.reacquisition)
