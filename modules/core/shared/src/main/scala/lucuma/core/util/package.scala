// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import coulomb.Quantity
import coulomb.ops.ShowUnit
import lucuma.core.math.dimensional.*
import lucuma.core.util.TypeString

package object util {
  opaque infix type Of[+T, U] <: T = T
  inline def tag[U]: Tagger[U] = Tagger()
  final class Tagger[U] {
    inline def apply[T](t: T): T Of U = t
  }

  extension[T](t: T)
    inline def tag[U]: T Of U = util.tag[U](t)
}
