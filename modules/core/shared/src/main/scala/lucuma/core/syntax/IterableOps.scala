// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import scala.collection.{IterableOps => ScalaIterableOps}

trait IterableOps:
  extension [S[_], A](aa:  ScalaIterableOps[Option[A], S, S[Option[A]]])
    /** Idiomatic way of reducing an `Iterable[Option[A]]` to `Iterable[A]` */
    def unNone: S[A] =  aa.flatten

object iterable extends IterableOps

