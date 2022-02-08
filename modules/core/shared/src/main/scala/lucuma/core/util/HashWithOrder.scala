// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.{Hash, Order}

trait HashWithOrder[A] extends Hash[A] with Order[A]

object HashWithOrder {

  @inline final def apply[A](implicit ev: HashWithOrder[A]): HashWithOrder[A] =
    ev

  def by[A, B](f: A => B)(implicit H: Hash[B], O: Order[B]): HashWithOrder[A] =
    new HashWithOrder[A] {
      override def compare(x: A, y: A): Int =
        O.compare(f(x), f(y))

      override def hash(x: A): Int =
        H.hash(f(x))
    }

}