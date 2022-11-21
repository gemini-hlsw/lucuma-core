// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

/**
 * Abstraction for optics that provide a function from `A` to either `B` or `E` (usually parsing and
 * validation), and a function from `B` back to `A` (usually formatting).
 *
 * Different implementations, like `ValidSplitEpi` and `ValidWedge` may have different sets of laws.
 */
trait ValidFormat[E, A, B] {
  val getValid: A => Either[E, B]

  val reverseGet: B => A
}
