// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.exposure

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.TimeSpan

import ExposureTimeViolation.Severity
import ExposureTimeViolation.formatSeconds

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

  /** The violation of this rule by `time`, if any. */
  def check(time: TimeSpan): Option[ExposureTimeViolation]

object ExposureRule:

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

    override def check(time: TimeSpan): Option[ExposureTimeViolation] =
      // e.g. "Exposure times above 1200 s are not recommended for GMOS North
      // due to cosmic ray contamination."
      def violation(limit: TimeSpan, below: Boolean): ExposureTimeViolation =
        val because     = reason.foldMap(r => s" ${r.value}")
        val description = severity match
          case Severity.Error   =>
            val bound = if below then "at least" else "at most"
            s"Exposure times for ${subject.value} must be $bound ${formatSeconds(limit)} s$because."
          case Severity.Warning =>
            val side = if below then "below" else "above"
            s"Exposure times $side ${formatSeconds(limit)} s are not recommended for ${subject.value}$because."
        ExposureTimeViolation(severity, description)

      limits.check(time) match
        case ExposureTimeLimits.Result.InRange    => none
        case ExposureTimeLimits.Result.Below(min) => violation(min, below = true).some
        case ExposureTimeLimits.Result.Above(max) => violation(max, below = false).some

  /** Exposure times must be a whole number of seconds. */
  final case class WholeSeconds(
    subject: NonEmptyString
  ) extends ExposureRule:

    override def severity: Severity = Severity.Error

    override def check(time: TimeSpan): Option[ExposureTimeViolation] =
      Option.when(time.toMicroseconds % 1_000_000L =!= 0L):
        ExposureTimeViolation(severity, s"Exposure times for ${subject.value} must be a whole number of seconds.")
