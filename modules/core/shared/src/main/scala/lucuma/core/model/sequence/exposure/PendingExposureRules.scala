// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.syntax.all.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.util.TimeSpan

/**
 * Finds the exposure times in a sequence step and the rules each must meet,
 * by passing the relevant parts of the step's configuration to
 * `ExposureRules`.  Use `ExposureTimeViolation.check` to check a step, so
 * that every caller applies the rules the same way.
 */
trait PendingExposureRules[D]:
  def pendingRules(step: Step[D], ctx: PendingExposureRules.Context): List[(TimeSpan, List[ExposureRule])]

object PendingExposureRules:

  def apply[D](using ev: PendingExposureRules[D]): PendingExposureRules[D] = ev

  /**
   * Observation-wide information that rules may depend on, beyond the step
   * itself.
   */
  final case class Context(
    constraints: ConstraintSet
  )

  given PendingExposureRules[Flamingos2DynamicConfig] = (step, _) =>
    List(step.instrumentConfig.exposure -> ExposureRules.flamingos2(step.instrumentConfig.readMode))

  given PendingExposureRules[GhostDynamicConfig] = (step, _) =>
    List(
      step.instrumentConfig.red.value.exposureTime  -> ExposureRules.ghostRed,
      step.instrumentConfig.blue.value.exposureTime -> ExposureRules.ghostBlue
    )

  // GMOS North and South differ only in their filter type and rules.  Imaging
  // exposures are science steps with neither a grating nor a focal plane unit;
  // `ExposureRules` decides which of them to check for saturation.
  private def gmos[F](
    step:       Step[?],
    exposure:   TimeSpan,
    readout:    GmosCcdMode,
    filter:     Option[F],
    hasGrating: Boolean,
    hasFpu:     Boolean
  )(rules: Option[ExposureRules.GmosImaging[F]] => List[ExposureRule]): List[(TimeSpan, List[ExposureRule])] =
    val isImaging = step.stepConfig === StepConfig.Science && !hasGrating && !hasFpu
    val imaging   = Option.when(isImaging):
      ExposureRules.GmosImaging(step.observeClass, filter, readout.xBin, readout.yBin, readout.ampGain)
    List(exposure -> rules(imaging))

  given PendingExposureRules[DynamicConfig.GmosNorth] = (step, ctx) =>
    val d = step.instrumentConfig
    gmos(step, d.exposure, d.readout, d.filter, d.gratingConfig.isDefined, d.fpu.isDefined):
      ExposureRules.gmosNorth(_, ctx.constraints.skyBackground)

  given PendingExposureRules[DynamicConfig.GmosSouth] = (step, ctx) =>
    val d = step.instrumentConfig
    gmos(step, d.exposure, d.readout, d.filter, d.gratingConfig.isDefined, d.fpu.isDefined):
      ExposureRules.gmosSouth(_, ctx.constraints.skyBackground)

  given PendingExposureRules[GnirsDynamicConfig] = (step, _) =>
    List(step.instrumentConfig.exposure -> ExposureRules.gnirs(step.instrumentConfig.readMode))

  given PendingExposureRules[Igrins2DynamicConfig] = (step, _) =>
    List(step.instrumentConfig.exposure -> ExposureRules.igrins2(step.observeClass))
