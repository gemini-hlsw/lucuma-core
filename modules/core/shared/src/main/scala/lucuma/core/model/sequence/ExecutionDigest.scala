// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.enums.ObserveClass
import monocle.Focus
import monocle.Lens

final case class ExecutionDigest(
  setup:       SetupDigest,
  acquisition: SequenceDigest,
  science:     SequenceDigest
):

  /**
   * ObserveClass computed from the ObserveClass of the science sequence atoms.
   */
  lazy val observeClass: ObserveClass =
    science.observeClass

  /**
   * Planned time for the observation, including the science sequence and an
   * estimated number of acquisitions.
   */
  def timeEstimate: CategorizedTime =
    science.timeEstimate.sumCharge(science.observeClass.chargeClass, setup.timeEstimate)

object ExecutionDigest:

  val Zero: ExecutionDigest =
    ExecutionDigest(
      SetupDigest.Zero,
      SequenceDigest.Zero,
      SequenceDigest.Zero
    )

  /** @group Optics */
  val setup: Lens[ExecutionDigest, SetupDigest] =
    Focus[ExecutionDigest](_.setup)

  /** @group Optics */
  val acquisition: Lens[ExecutionDigest, SequenceDigest] =
    Focus[ExecutionDigest](_.acquisition)

  /** @group Optics */
  val science: Lens[ExecutionDigest, SequenceDigest] =
    Focus[ExecutionDigest](_.science)

  given Eq[ExecutionDigest] =
    Eq.by(a => (a.setup, a.acquisition, a.science))