// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import lucuma.core.util.Display

final class DisplayOps[A](private val value: A) extends AnyVal {

  def shortName(implicit d: Display[A]): String = d.shortName(value)
  def longName(implicit d: Display[A]): String  = d.longName(value)
}

trait ToDisplayOps {
  implicit def ToDisplayOps[A](value: A): DisplayOps[A] =
    new DisplayOps[A](value)
}

object display extends ToDisplayOps
