// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import java.time.Instant
import gsp.math.optics.Wedge
import gsp.math.optics.SplitEpi
import gsp.math.optics.SplitMono
import monocle.Iso

object GetterStrategy {
  sealed trait Closest
  sealed trait LinearInterpolating
}

/**
  * Typeclass defininig how to get a value from a [[Calculator]] using a certain [[GetterStrategy]].
  *
  * @tparam G [[GetterStrategy]] to use
  * @tparam A type of the value to get
  */
trait CalcGetter[G, A] { self =>

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
  )(field: T => A)(instant: Instant): A

  def imap[B](f: A => B)(g: B => A): CalcGetter[G, B] =
    new CalcGetter[G, B] {
      def get[T](
        calc:  Calculator[G, T]
      )(field: T => B)(instant: Instant): B =
        f(self.get(calc)(field.andThen(g))(instant))
    }

  def imap[B](optic: Wedge[A, B]): CalcGetter[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitEpi[A, B]): CalcGetter[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: SplitMono[A, B]): CalcGetter[G, B] =
    imap(optic.get)(optic.reverseGet)

  def imap[B](optic: Iso[A, B]): CalcGetter[G, B] =
    imap(optic.get _)(optic.reverseGet)
}
