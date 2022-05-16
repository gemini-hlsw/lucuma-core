// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval

import scala.compiletime.constValue

inline def refineMV[T, P](inline t: T)(using inline p: Predicate[T, P]): Refined[T, P] = {
  inline if (p.isValid(t)) Refined.unsafeApply(t) else scala.compiletime.error("no")
}

trait Predicate[T, P] {
  transparent inline def isValid(inline t: T): Boolean
}

object Predicate {

  inline given [M <: Int, N <: Int]: Predicate[Int, Interval.Closed[M, N]] with
    transparent inline def isValid(inline t: Int): Boolean = constValue[M] <= t && t <= constValue[N]
}