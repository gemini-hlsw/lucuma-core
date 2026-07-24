// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import munit.BaseFunSuite
import munit.Flaky

import scala.concurrent.Future

/**
 * Mixin that retries tests tagged as flaky (i.e. defined with `test("...".flaky)`).
 *
 * Retries only happen in CI (override [[munitRetryFlaky]] to change).
 *
 * Limited to [[munitFlakyAttempts]] attempts (default 3).
 */
trait RetryFlakyTests extends BaseFunSuite:

  /** Total number of attempts for a flaky test */
  protected def munitFlakyAttempts: Int = 3

  /** Whether to retry flaky tests. By default, this is true in CI and false locally. */
  protected def munitRetryFlaky: Boolean = sys.env.contains("CI")

  override def munitTestTransforms: List[TestTransform] =
    if (munitRetryFlaky) super.munitTestTransforms :+ retryFlakyTransform
    else super.munitTestTransforms

  private def retryFlakyTransform: TestTransform =
    new TestTransform(
      "retry flaky tests",
      test =>
        if (test.tags(Flaky)) test.withBody(() => runWithRetries(test, attempt = 1))
        else test
    )

  private def runWithRetries(test: Test, attempt: Int): Future[Any] =
    test
      .body()
      .recoverWith:
        case _ if attempt < munitFlakyAttempts =>
          println(s"FAILED: ${test.name} (attempt $attempt/$munitFlakyAttempts)")
          runWithRetries(test, attempt + 1)
      (using munitExecutionContext)
