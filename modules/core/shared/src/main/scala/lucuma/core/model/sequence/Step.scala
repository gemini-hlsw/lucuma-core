// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan
import lucuma.core.util.WithUid
import lucuma.refined.*
import monocle.Focus
import monocle.Lens

/**
 * Complete step configuration for a particular instrument's dynamic config.
 *
 * @tparam D dynamic config type (e.g., DynamicConfig.GmosNorth)
 */
case class Step[D](
  id:                Step.Id,
  instrumentConfig:  D,
  stepConfig:        StepConfig,
  estimate:          StepEstimate,
  observeClass:      ObserveClass = ObserveClass.Science,
  breakpoint:        Breakpoint = Breakpoint.Disabled,
) {

  lazy val timeEstimate: CategorizedTime =
    CategorizedTime(observeClass.chargeClass -> estimate.total)

}

object Step extends WithUid('s'.refined) {

  /** @group Optics */
  def id[D]: Lens[Step[D], Step.Id] =
    Focus[Step[D]](_.id)

  /** @group Optics */
  def instrumentConfig[D]: Lens[Step[D], D] =
    Focus[Step[D]](_.instrumentConfig)

  /** @group Optics */
  def stepConfig[D]: Lens[Step[D], StepConfig] =
    Focus[Step[D]](_.stepConfig)

  /** @group Optics */
  def estimate[D]: Lens[Step[D], StepEstimate] =
    Focus[Step[D]](_.estimate)

  /** @group Optics */
  def observeClass[D]: Lens[Step[D], ObserveClass] =
    Focus[Step[D]](_.observeClass)

    /** @group Optics */
  def breakpoint[D]: Lens[Step[D], Breakpoint] =
    Focus[Step[D]](_.breakpoint)

  given [D](using Eq[D]): Eq[Step[D]] =
    Eq.by { x => (
      x.id,
      x.instrumentConfig,
      x.stepConfig,
      x.estimate,
      x.observeClass,
      x.breakpoint
    )}

}
