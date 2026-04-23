// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import cats.Hash
import cats.Order

package object util {
  opaque infix type Of[+T, U] <: T = T
  inline def tag[U]: Tagger[U] = Tagger()
  final class Tagger[U] {
    inline def apply[T](t: T): T Of U = t
  }

  extension [T](t: T) inline def tag[U]: T Of U = util.tag[U](t)

  /**
   * A typeclass representing both `Order` and `Hash`. Necessary since both extend `Eq` and when
   * both are present, `Eq` is ambiguous. This class solves the ambiguity by forcing `Eq` through
   * `Order` and ignoring the `Hash` instance for equality.
   */
  class OrderHash[V](ord: Order[V], hash: Hash[V]) extends Order[V] with Hash[V]:
    override def eqv(x:     V, y: V): Boolean = ord.eqv(x, y)
    override def compare(x: V, y: V): Int     = ord.compare(x, y)
    override def hash(x:    V): Int           = hash.hash(x)
}
