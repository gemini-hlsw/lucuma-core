// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.tests

// munit's regular .flaky tag doesn't catch `AssertionError`s thrown by scalacheck,
// so we define a custom tag to skip these tests when flaky tests are OK.
object ScalaCheckFlaky extends munit.Tag("ScalaCheckFlaky")
