// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.Order
import cats.syntax.all.*
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

/**
 * The violation of an exposure rule: whether it is an error or a warning, and
 * a sentence stating the rule.  It says nothing of the exposure time that
 * broke the rule, which the caller checking it already knows, so a sequence
 * that breaks a rule in many steps has just one violation of it.
 *
 * @param description e.g. "Exposure times for GMOS North must be at least 1 s."
 */
final case class ExposureTimeViolation(
  severity:    ExposureTimeViolation.Severity,
  description: String
):

  def isError: Boolean =
    severity === ExposureTimeViolation.Severity.Error

object ExposureTimeViolation:

  /**
   * Whether a violation means the exposure cannot be taken (`Error`) or is
   * merely outside the recommended range (`Warning`).
   */
  enum Severity(val tag: String) derives Enumerated:
    case Error   extends Severity("error")
    case Warning extends Severity("warning")

  /** Errors first, then by description. */
  given Order[ExposureTimeViolation] =
    Order.by(v => (v.severity, v.description))

  given Ordering[ExposureTimeViolation] =
    Order[ExposureTimeViolation].toOrdering

  /** Seconds without trailing zeros, e.g. "0.5" or "1200". */
  def formatSeconds(time: TimeSpan): String =
    time.toSeconds.bigDecimal.stripTrailingZeros.toPlainString

  /**
   * The violations of `rules` by an exposure of `time` with the given observe
   * class.  Errors are reported if there are any.  Otherwise warnings are
   * reported, but for science exposures alone, so calibrations and
   * acquisitions are only checked for errors.  Use this to check an exposure
   * time against rules from `ExposureRules` directly, so that the rules are
   * applied in the same way as for a sequence step.
   */
  def check(
    rules:        List[ExposureRule],
    time:         TimeSpan,
    observeClass: ObserveClass
  ): List[ExposureTimeViolation] =
    val (errorRules, warningRules) = rules.partition(_.isError)
    errorRules.flatMap(_.check(time)) match
      case Nil if observeClass === ObserveClass.Science => warningRules.flatMap(_.check(time))
      case errors                                       => errors

  /**
   * The violations in a step, as for the exposure time version.  Bias steps
   * are skipped since they take no exposure.
   */
  def check[D: PendingExposureRules](
    step: Step[D],
    ctx:  PendingExposureRules.Context
  ): List[ExposureTimeViolation] =
    step.stepConfig match
      case StepConfig.Bias => Nil
      case _               =>
        PendingExposureRules[D].pendingRules(step, ctx).flatMap: (time, rules) =>
          check(rules, time, step.observeClass)
