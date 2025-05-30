// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

/** Typeclass for data types that wrap an `Angle`. */
trait Angular[A]:
  extension (a: A) def toAngle: Angle

object Angular:

  def instance[A](f: A => Angle): Angular[A] =
    new Angular[A]:
      extension (a: A) def toAngle: Angle = f(a)

  given Angular[Angle] = instance(identity)
  given Angular[RightAscension] = instance(_.toAngle)
  given Angular[Declination] = instance(_.toAngle)