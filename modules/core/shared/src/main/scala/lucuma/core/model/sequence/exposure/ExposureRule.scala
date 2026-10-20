// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.sequence.StepViolation
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

import ExposureRule.Severity

/**
 * A requirement on an exposure time in some configuration.  Breaking a rule
 * that is an error means the exposure cannot be taken; breaking one that is
 * not is legal but unusual, and merits a warning.  A rule depends only on the
 * configuration (and, for GMOS imaging, the sky background), never on the
 * exposure time it checks.  A single exposure is usually subject to several
 * rules.
 */
sealed trait ExposureRule derives Eq:

  /** What to call the configuration in a message, e.g. "GMOS North". */
  def subject: NonEmptyString

  def severity: Severity

  def isError: Boolean =
    severity === Severity.Error

  /**
   * The validation code a violation of this rule is reported with: an
   * exposure that cannot be taken is a configuration error, one merely
   * outside the recommended range a (dismissable) exposure time warning.
   */
  def code: ObservationValidationCode =
    severity match
      case Severity.Error   => ObservationValidationCode.ConfigurationError
      case Severity.Warning => ObservationValidationCode.ExposureTimeWarning

  /**
   * The violation of this rule by `time`, if any, stating the rule as a
   * sentence.  It says nothing of the exposure time that broke the rule, so a
   * sequence that breaks a rule in many steps has just one violation of it.
   */
  def check(time: TimeSpan): Option[StepViolation]

object ExposureRule:

  /**
   * Whether breaking a rule means the exposure cannot be taken (`Error`) or is
   * merely outside the recommended range (`Warning`).
   */
  enum Severity(val tag: String) derives Enumerated:
    case Error   extends Severity("error")
    case Warning extends Severity("warning")

  /** Seconds without trailing zeros, e.g. "0.5" or "1200". */
  def formatSeconds(time: TimeSpan): String =
    time.toSeconds.bigDecimal.stripTrailingZeros.toPlainString

  /**
   * Exposure times must fall within `limits`.
   *
   * @param reason why the limits apply, if worth saying, e.g. "due to cosmic
   *               ray contamination"
   */
  final case class Limits(
    subject:  NonEmptyString,
    reason:   Option[NonEmptyString],
    severity: Severity,
    limits:   ExposureTimeLimits
  ) extends ExposureRule:

    override def check(time: TimeSpan): Option[StepViolation] =
      // e.g. "Exposure times above 1200 s are not recommended for GMOS North
      // due to cosmic ray contamination."
      def violation(limit: TimeSpan, below: Boolean): StepViolation =
        val because     = reason.foldMap(r => s" ${r.value}")
        val description = severity match
          case Severity.Error   =>
            val bound = if below then "at least" else "at most"
            s"Exposure times for ${subject.value} must be $bound ${formatSeconds(limit)} s$because."
          case Severity.Warning =>
            val side = if below then "below" else "above"
            s"Exposure times $side ${formatSeconds(limit)} s are not recommended for ${subject.value}$because."
        StepViolation(code, description)

      limits.check(time) match
        case ExposureTimeLimits.Result.InRange    => none
        case ExposureTimeLimits.Result.Below(min) => violation(min, below = true).some
        case ExposureTimeLimits.Result.Above(max) => violation(max, below = false).some

  /** Exposure times must be a whole number of seconds. */
  final case class WholeSeconds(
    subject: NonEmptyString
  ) extends ExposureRule:

    override def severity: Severity = Severity.Error

    override def check(time: TimeSpan): Option[StepViolation] =
      Option.when(time.toMicroseconds % 1_000_000L =!= 0L):
        StepViolation(code, s"Exposure times for ${subject.value} must be a whole number of seconds.")
