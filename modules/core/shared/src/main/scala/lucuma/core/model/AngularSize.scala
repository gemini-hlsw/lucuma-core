// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import lucuma.core.math.Angle
import monocle.Focus
import monocle.Lens

/**
 * Angular size of an object in the sky. The major axis is the longest one, but we don't enforce
 * this.
 */
final case class AngularSize(majorAxis: Angle, minorAxis: Angle)

object AngularSize {
  val majorAxis: Lens[AngularSize, Angle] = Focus[AngularSize](_.majorAxis)
  val minorAxis: Lens[AngularSize, Angle] = Focus[AngularSize](_.minorAxis)

  /** @group Typeclass Instances */
  implicit val AngularSizeShow: Show[AngularSize] =
    Show.fromToString

  /** @group Typeclass Instances */
  implicit val AngularSizeEqual: Eq[AngularSize] =
    Eq.fromUniversalEquals
}
