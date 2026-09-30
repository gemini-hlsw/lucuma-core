// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.monoid.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

case class ExecutionDigest(
  setup:              SetupTime,
  setupCount:         NonNegInt,
  reacquisitionCount: NonNegInt,
  calibrationCount:   NonNegInt,
  acquisition:        SequenceDigest,
  science:            SequenceDigest
) {

  /**
   * ObserveClass computed from the ObserveClass of the science sequence atoms.
   */
  lazy val observeClass: ObserveClass =
    science.observeClass

  /**
   * Total setup time for the observation: every expected full setup plus
   * every expected reacquisition.
   */
  def totalSetupTime: TimeSpan =
    (setup.full *| setupCount.value) +| (setup.reacquisition *| reacquisitionCount.value)

  /**
   * Planned time for the observation, including the science sequence and all
   * expected acquisitions and reacquisitions.
   */
  def fullTimeEstimate: CategorizedTime =
    science.timeEstimate.sumCharge(science.observeClass.chargeClass, totalSetupTime)

  /**
   * Steps by kind across the acquisition and science sequences.  Excludes
   * setup time.
   */
  def steps: StepDigests =
    acquisition.steps |+| science.steps

}

object ExecutionDigest {

  val Zero: ExecutionDigest =
    ExecutionDigest(
      SetupTime.Zero,
      NonNegInt.MinValue,
      NonNegInt.MinValue,
      NonNegInt.MinValue,
      SequenceDigest.Zero,
      SequenceDigest.Zero
    )

  /** @group Optics */
  val setup: Lens[ExecutionDigest, SetupTime] =
    Focus[ExecutionDigest](_.setup)

  /** @group Optics */
  val setupCount: Lens[ExecutionDigest, NonNegInt] =
    Focus[ExecutionDigest](_.setupCount)

  /** @group Optics */
  val reacquisitionCount: Lens[ExecutionDigest, NonNegInt] =
    Focus[ExecutionDigest](_.reacquisitionCount)

  /** @group Optics */
  val calibrationCount: Lens[ExecutionDigest, NonNegInt] =
    Focus[ExecutionDigest](_.calibrationCount)

  /** @group Optics */
  val acquisition: Lens[ExecutionDigest, SequenceDigest] =
    Focus[ExecutionDigest](_.acquisition)

  /** @group Optics */
  val science: Lens[ExecutionDigest, SequenceDigest] =
    Focus[ExecutionDigest](_.science)

  given Eq[ExecutionDigest] =
    Eq.by { a => (
      a.setup,
      a.setupCount,
      a.reacquisitionCount,
      a.calibrationCount,
      a.acquisition,
      a.science
    )}

}