// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

/**
  * Typeclass for things that can be shown in a user interface.
  * @group Typeclasses
  */
trait Display[A]     {
  def shortName(a: A): String
  def longName(a:  A): String = shortName(a)
}
