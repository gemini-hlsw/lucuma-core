// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import java.time.Instant

object GetterStrategy {
  sealed trait Exact
  sealed trait LinearInterpolating
}

/**
  * Typeclass defininig how to get a value from a [[Calculator]] using a certain [[GetterStrategy]].
  *
  * @tparam G [[GetterStrategy]] to use
  * @tparam A type of the value to get
  */
trait CalcGetter[G, A] {

  /**
    * Get value from a [[Calculator]].
    *
    * @tparam T the results that the [[Calculator]] holds
    * @param calc [[Calculator]] holding the timed results of type [[T]]
    * @param field how to extract a partial result of type [[A]] from a [[T]]
    * @param instant the desired [[java.time.Instant]] at which to get the value of type [[A]]
    */
  def get[T](
    calc:  Calculator[G, T]
  )(field: T => A)(instant: Instant): A {
    // Wedge???
  }
}
