// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import lucuma.core.enums.ObserveClass
import monocle.Focus
import monocle.Lens

case class ExecutionDigest(
  setup:       SetupTime,
  acquisition: SequenceDigest,
  science:     SequenceDigest
) {

  /**
   * ObserveClass computed from the ObserveClass of the science sequence atoms.
   */
  lazy val observeClass: ObserveClass =
    science.observeClass

  /**
   * Planned time for the observation, including the science sequence but only
   * a reacquisition cost instead of the full setup time.
   */
  def reacquisitionTimeEstimate: CategorizedTime =
    science.timeEstimate.sumCharge(science.observeClass.chargeClass, setup.reacquisition)

  /**
   * Planned time for the observation, including the science sequence and a
   * full setup time.
   */
  def fullTimeEstimate: CategorizedTime =
    science.timeEstimate.sumCharge(science.observeClass.chargeClass, setup.full)

}

object ExecutionDigest {

  val Zero: ExecutionDigest =
    ExecutionDigest(
      SetupTime.Zero,
      SequenceDigest.Zero,
      SequenceDigest.Zero
    )

  /** @group Optics */
  val setup: Lens[ExecutionDigest, SetupTime] =
    Focus[ExecutionDigest](_.setup)

  /** @group Optics */
  val acquisition: Lens[ExecutionDigest, SequenceDigest] =
    Focus[ExecutionDigest](_.acquisition)

  /** @group Optics */
  val science: Lens[ExecutionDigest, SequenceDigest] =
    Focus[ExecutionDigest](_.science)

  given Eq[ExecutionDigest] =
    Eq.by { a => (
      a.setup,
      a.acquisition,
      a.science
    )}

}